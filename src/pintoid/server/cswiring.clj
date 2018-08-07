(ns pintoid.server.cswiring
  (:use
   pintoid.server.game.core
   [pintoid.server utils ecs])
  (:require
   [mount.core :refer [defstate]]
   [taoensso.timbre :as timbre]
   [clojure.data.int-map :as im]
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
  (next-entity-id :player))

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
  a)

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
    (alter-avatar pid handle-client-disconnected pid)
    (<! (timeout client-destroy-timeout))
    (alter-avatar pid avatar-destroy-when-disconnected)))

(defn attach-ws-connection [pid wsc]
  (timbre/debugf "Attach new websocket to avatar %s" pid)
  (alter-avatar
   pid
   (fn [a]
     (when-let [cd (:cancel-destroy a)] (cd))
     (-> a
         (avatar-close-ws-chans)
         (assoc
          :dumpstate nil  ;; reset all dumpers
          :cancel-destroy nil
          :ws-channel wsc
          :ws-reader (spawn-wschan-reading-loop pid wsc))))))


;; == Send world patch

(declare make-world-dumper)

(defn- create-and-send-world-patch [a pid at w]
  (if (and (w pid :player) (:ws-channel a))
    (let [ss (:dumpstate a)
          wd (make-world-dumper pid w)
          [ss' dump] (wd ss w)
          wpatch (into {} (map (fn [[c d]] [c (vec d)])) dump)]
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
      (timbre/tracef "player %s not in game" (entity w pid))
      a)))

(defn send-snapshots-to-all-clients []
  (let [[at w] (get-world)]
    (dosync
     (doseq [[pid a] @avatars]
       (send-off a create-and-send-world-patch pid at w)))))

(defn combine-dumpers* [dumpers]
  (fn [state w]
    (let [sds (map
               (fn [[c df]]
                 (let [s (get state c)
                       [s' d] (df s w)]
                   [{c s'} d]))
               dumpers)
          states (into {} (map first sds))
          dumps  (apply merge-with #(eduction cat [%1 %2]) (map second sds))]
    [states dumps])))

(defmacro combine-dumpers [& dumpers]
  "Combine several dumpers into one."
  `(combine-dumpers*
    ~(zipmap (repeatedly #(keyword (gensym "ecs_dumper"))) dumpers)))

(defn dumper [& {cid :cid eids :eids mapf :convert feid :feid fval :filter}]
  "Returns dumper function [state world] => [new-state {:cid1 [...], :cid2 [...]}]
   Arguments:
      :cid  component name (mandatory)
      :eids bitset of entity ids
      :feid filter eids
      :filter filter pairs [eid value]
      :convert convert component values
  "
  (fn [{cm' :compmap d' :dump :or {d' {}} :as state} w]
    (let [cm (component-map w cid)]
      (if (= cm' cm)
        [state {}]
        (let [changed? (fn [[eid s]] (not (when-let [s' (d' eid)] (= s s'))))
              ef (cond-> identity
                   feid (comp (filter feid))
                   true (comp (map (fn [eid] [eid (cm eid)])))
                   true (comp (filter changed?))
                   fval (comp (filter fval)))
              eids (cond->> (entity-ids w cid) eids (eids* eids))
              d-add (into {} ef eids)
              removed? (fn [eid]
                         (and
                          (not (contains? d-add eid))
                          (or
                           (not (contains? eids eid))
                           (and feid (not (feid eid)))
                           (and fval (not (fval (find cm eid)))))))
              d-rem (into [] (comp (map key) (filter removed?)) d')
              d (as-> d' $ (apply dissoc $ d-rem) (into $ d-add))]
          [(assoc state :compmap cm :dump d)
           {cid (eduction
                 cat [(eduction (map (fn [x] [x nil])) d-rem)
                      (if mapf
                        (eduction (map (fn [[k v]] [k (mapf v)])) d-add)
                        d-add)])}])))))

(defn dumper-only-visible-entities [pid w]
  (let [player-xy (w pid :position)
        is-visible? #(when-let [f (w % :visible?)] (f w pid %))
        visible (into (im/dense-int-set) (filter is-visible?) (entity-ids w :position))]
    (combine-dumpers
     (dumper :cid :type, :eids visible)
     (dumper :cid :position, :convert (juxt :x :y), :eids visible)
     (dumper :cid :position-tts, :eids visible)
     (dumper :cid :angle, :eids visible)
     (dumper :cid :layer, :eids visible)
     (dumper :cid :sprite, :eids visible))))

(defn dumper-self-player [pid]
  (fn [sent w]
    (if sent
      [true nil]
      [true {:self-player [[pid true]]}])))

(defn make-world-dumper [pid w]
  (combine-dumpers
   (dumper-only-visible-entities pid w)
   (dumper :cid :score)
   (dumper :cid :assets)
   (dumper-self-player pid)
   ))
