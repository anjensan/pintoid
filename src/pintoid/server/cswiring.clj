(ns pintoid.server.cswiring
  (:use
   pintoid.server.game
   pintoid.ecs.core)
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
  (next-entity))

(defn- create-empty-avatar [pid]
  (agent {:pid pid}))

(defn create-player-avatar [pid {:keys [ip nick]}]
  (timbre/tracef "Create avatar %s" pid)
  (send avatars
   (fn [as]
     (let [a (or (get as pid)
                 (create-empty-avatar pid))]
       (send a assoc :host ip :nick nick)
       (assoc as pid a)))))

(defn- avatar-close-ws-chans [a]
  (when-let [wc (:ws-channel a)] (close! wc))
  (when-let [cr (:ws-reader a)] (close! cr))
  (dissoc a :ws-channel :ws-reader))

(defn- avatar-destroy [{pid :pid :as a}]
  (timbre/debugf "Destroy avatar for %s" pid)
  (when (contains? @avatars pid)
    (timbre/warnf "Avatar %s has not been removed from 'avatars'!" pid))
  (avatar-close-ws-chans a)
  (game-remove-player pid)
  {:pid pid :state ::destroyed})

(defn- cleanup-avatars [as]
  (timbre/debugf "Destroy avatars %s" as)
  (send avatars #(doseq [[_ a] %] (send a avatar-destroy))) {})

(defn destroy-player-avatar [pid]
  (send
   avatars
   (fn [as]
     (when-let [a (get as pid)]
       (send a avatar-destroy)
       (dissoc as pid)))))

(defn send-to-client [pid message]
  (alter-avatar pid
   (fn [a]
     (when-let [c (:ws-channel a)]
       (timbre/tracef "Message %s << %s" pid message)
       (go (>! (:ws-channel a) message)))
     a)))

(defmulti handle-client-message
  (fn [a message] (:command message)))

(defmethod handle-client-message :default [a m]
  (timbre/warnf "Unknown message from %s" (:pid a))
  a)

(defmethod handle-client-message :join-game [a m]
  (timbre/infof "Player '%s' joined the game" (:nick a))
  (game-add-new-player (:pid a) {:nick (:nick a)})
  a)

(defmethod handle-client-message :user-input [a m]
  (timbre/tracef "User %s send input %s" (:pid a) m)
  (game-process-user-input (:pid a) (:data m))
  (assoc a :user-input (:data m)))

(defn handle-client-chan-error [a err]
  (timbre/warnf "Client %s: %s" (:pid a) err)
  a)

(defn handle-client-disconnected [a]
  (timbre/infof "Player '%s' disconnected" (:nick a))
  (dissoc a :ws-channel))

(defn- avatar-destroy-when-disconnected [pid]
  (when-let [a (get @avatars pid)]
    (let [{wsc :ws-channel} @a]
      (when-not wsc
        (destroy-player-avatar pid)))))

(defn- spawn-wschan-reading-loop [pid ws-channel]
  (go
    (loop []
      (let [{:keys [message error] :as raw} (<! ws-channel)]
        (cond
          error      (alter-avatar pid handle-client-chan-error error)
          message    (do
                      (timbre/tracef "Message %s >> %s" pid message)
                      (alter-avatar pid handle-client-message message)
                      (recur)))))
    (alter-avatar pid handle-client-disconnected)
    (<! (timeout client-destroy-timeout))
    (avatar-destroy-when-disconnected pid)))

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

(defn- create-and-send-world-patch [a pid at w]
  (timbre/tracef "Create and send world patch for %s, at %s" pid at)
  (if (and (w pid :player) (:ws-channel a))
    (let [ss (:dumpstate a)
          [d ss'] ((dump-the-world w pid) ss)
          wpatch (into {} (comp
                           (map (fn [[k v]] [k (vec v)]))
                           (remove (fn [[k v]] (empty? v))))
                       d)]
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
      (timbre/tracef "Player %s not in game" pid)
      a)))

(defn send-snapshots-to-all-clients [done]
  (let [[at w] (get-world)]
    (send
     avatars
     (fn [as]
       (timbre/trace "Send snapthots to all clients")
       (doseq [[pid a] as]
         (send-off a create-and-send-world-patch pid at w))
       (done)
       as))))
