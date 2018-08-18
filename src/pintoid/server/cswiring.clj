(ns pintoid.server.cswiring
  (:use
   clojure.algo.monads
   pintoid.server.game.core
   [pintoid.server utils ecs])
  (:require
   [clojure.data.int-map :as im]
   [mount.core :refer [defstate]]
   [taoensso.timbre :as timbre]
   [clojure.core.async :refer [<! >! close! go go-loop timeout chan alt!]]))

(def client-destroy-timeout 10000)

(declare cleanup-avatars)

(defstate avatars
  :start (agent {})
  :stop (cleanup-avatars))

(defn- alter-avatar [pid f & rs]
  (let [a (get @avatars pid)]
    (if a
      (apply send a f rs)
      (timbre/debugf "Unknown pid %s, skip %s" pid f))))

(defn generate-player-pid []
  (next-entity :player))

(defn- create-empty-avatar [pid]
  (agent {:pid pid}))

(defn- populate-avatar-with-req [a req]
  (timbre/tracef "Update avatar %s with data from req %s" a req)
  (send a assoc
         :host (:remote-addr req)
         :name (get-in req [:session :name])))

(defn create-player-avatar [pid req]
  (send avatars
        (fn [as]
          (let [a (or (get as pid)
                      (create-empty-avatar pid))]
            (populate-avatar-with-req a req)
            (assoc as pid a)))))


(defn- avatar-close-ws-chans [a]
  (when-let [wc (:ws-channel a)] (close! wc))
  (when-let [cr (:ws-reader a)] (close! cr))
  (dissoc a :ws-channel :ws-reader))

(defn- avatar-destroy [{pid :pid :as a}]
  (timbre/debugf "Destroy avatar for %s" pid)
  (when (contains? @avatars pid)
    (timbre/warnf "Avatar %s has not been removed from 'avatars'!"))
  (avatar-close-ws-chans a)
  (game-remove-player pid)
  {:pid pid :state ::destroyed})

(defn- cleanup-avatars [as]
  (send avatars #(doseq [[_ a] %] (send a avatar-destroy))) {})

(defn destroy-player-avatar [pid]
  (send avatars
        (fn [as]
          (when-let [a (get as pid)]
            (send a avatar-destroy)
            (dissoc as pid)))))

(defn send-to-client [pid message]
  (alter-avatar pid
   (fn [a]
     (when-let [c (:ws-channel a)]
       (timbre/tracef "To client %s: %s" pid message)
       (go (>! (:ws-channel a) message)))
     a)))

(defmulti handle-client-message
  (fn [a message] (:command message)))

(defmethod handle-client-message :default [a m]
  (timbre/warnf "Unknown message from %s" (:pid a))
  a)

(defmethod handle-client-message :join-game [a m]
  (timbre/infof "Player %s joined the game" (:pid a))
  (game-add-new-player (:pid a))
  a)

(defmethod handle-client-message :user-input [a m]
  (timbre/tracef "User %s send input %s" (:pid a) m)
  (game-process-user-input (:pid a) (:data m))
  (assoc a :user-input (:data m)))

(defn handle-client-chan-error [a err]
  (timbre/warnf "Client %s: %s" (:pid a) err)
  a)

(defn handle-client-disconnected [a]
  (timbre/infof "Client %s disconnected" (:pid a))
  (dissoc a :ws-channel))

(defn- avatar-destroy-when-disconnected [{wsc :ws-channel :as a}]
  (if wsc a (avatar-destroy a)))

(defn- spawn-wschan-reading-loop [pid ws-channel]
  (go
    (loop []
      (let [{:keys [message error] :as raw} (<! ws-channel)]
        (cond
          error      (alter-avatar pid handle-client-chan-error error)
          message    (do
                      (timbre/tracef "From client %s: %s" pid message)
                      (alter-avatar pid handle-client-message message)
                      (recur)))))
    (alter-avatar pid handle-client-disconnected)
    (<! (timeout client-destroy-timeout))
    (alter-avatar pid avatar-destroy-when-disconnected)))

(declare dump-the-world)

(defn attach-ws-connection [pid wsc]
  (timbre/debugf "Attach new websocket to avatar %s" pid)
  (alter-avatar
   pid
   (fn [a]
     (when-let [cd (:cancel-destroy a)] (cd))
     (-> a
         (avatar-close-ws-chans)
         (assoc
          :dumpstate nil  ;; reset all df-map
          :cancel-destroy nil
          :ws-channel wsc
          :ws-reader (spawn-wschan-reading-loop pid wsc))))))


;; == Send world patch

(defn- create-and-send-world-patch [a pid at w]
  (if (and (w pid :player) (:ws-channel a))
    (let [ss (:dumpstate a)
          [d ss'] ((dump-the-world w pid) ss)
          wpatch (into {} (comp (map-val vec) (remove (comp empty? second))) d)]
      (timbre/tracef "send to %s wpatch %s" pid wpatch)
      (send-to-client
       pid
       {:server-time (System/currentTimeMillis)
        :game-time at
        :command :wpatch
        :self pid
        :ecs wpatch})
      (assoc a
             :actual-world w
             :dumpstate ss'))
    (do
      (timbre/tracef "player %s not in game" pid)
      a)))

(defn send-snapshots-to-all-clients []
  (let [[at w] (get-world)]
    (dosync
     (doseq [[pid a] @avatars]
       (send-off a create-and-send-world-patch pid at w)))))


;; Dumpers

(with-monad (maybe-t state-m ::empty-dump)

  (defn d-map [f dm]
    (domonad [d dm] (eduction (map-val f) d)))

  (defn d-filter [p dm]
    (domonad [d dm] (eduction (filter p) d)))

  (defn d-diff [dm]
    (domonad [new ((m-lift 1 to-int-map) dm)
              old (set-val ::d-diff-old new)]
      (edcat
        (eduction (comp (remove #(contains? new %)) (map #(vector % nil))) (keys old))
        (eduction (remove (fn [[k v]] (= v (get old k)))) new))))

  (defn d-when-not-identical [dm]
    (domonad [new dm, old (set-val ::d-changed-old new)]
      (if (identical? old new) ::empty-dump new)))
  )


(with-monad state-m

  (defn dumps-map [& [k m & r :as rr]]
    (if-not (seq rr)
      (m-result {})
      (domonad [v (with-state-field k m)
                z (apply dumps-map r)]
        (if-not (= v ::empty-dump)
          (assoc z k v)
          z))))

  (defn dump
    [w c & {filter :filter
            map :map
            diff :diff
            icheck :ident-check
            :or {diff true, icheck true}}]
    (cond->> (m-result (get-comp-map w c))
      icheck (d-when-not-identical)
      filter (d-filter filter)
      diff   (d-diff)
      map    (d-map map)))
  )


;; === Logic

(defn roundf [^double x ^double f]
  (-> x (* f) (Math/round) (double) (/ f)))

(defn serialize-vec2 [xy]
  (when xy
    [(roundf (:x xy) 100)
     (roundf (:y xy) 100)]))

(defn make-visibility-filter [w pid]
  (let [v? #(when-let [f (w % :visible?)] (f w pid %))
        eids (into #{} (filter v?) (entities w :position))]
    (fn [[eid _]] (contains? eids eid))))

(defn dump-self-player [pid]
  (domonad state-m [sent (set-val :sent true)]
    (when-not sent {pid true})))

(defn dump-the-world [w pid]
  (let [vf (make-visibility-filter w pid)]
    (dumps-map
      :self-player  (dump-self-player pid)
      :assets       (dump w :assets)
      :score        (dump w :score)
      :position     (dump w :position, :filter vf, :map serialize-vec2)
      :position-tts (dump w :position-tts, :filter vf)
      :sprite       (dump w :sprite, :filter vf)
      :layer        (dump w :layer, :filter vf)
      :angle        (dump w :angle, :filter vf)
      :type         (dump w :type, :filter vf)
      )))

