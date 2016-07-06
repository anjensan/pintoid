(ns pintoid.server.cswiring
  (:use
   [pintoid.server game utils ecs math])
  (:require
   [taoensso.timbre :as timbre]
   [clojure.data.int-map :as im]
   [clojure.core.async :refer
    [<! >! <!! >!! put! close! thread go chan go-loop]]
   [cheshire.core :as json]
   [clojure.tools.logging :as log]
   [clojure.set :refer [union]]))


(def avatars (ref {}))

(defn create-avatar [pid req]
  (timbre/trace "create avatar" pid req)
  (agent
   {:pid pid
    :ws-channel (:ws-channel req)}))


(defn- send-message-to-client [pid message]
  (send
   (get @avatars pid)
   (fn [avatar]
     (timbre/trace "pid" pid ">>" message)
     (go (>! (:ws-channel avatar) message))
     avatar)))

(defn drop-client-connection! [eid]
  (when-let [a (get @avatars eid)]
    (send a (fn [a] (close! (:ws-channel a) a)))))


(declare handle-client-message)
(declare on-client-disconnected)

(defn- spawn-wschan-reading-loop [eid ws-channel]
  (go-loop []
    (if-let [{:keys [message error]} (<! ws-channel)]
      (do
        (when error
          (timbre/warn "eid" eid "!!" error))
        (when message
          (timbre/trace "eid" eid "<<" message)
          (send
           (get @avatars eid)
           (fn [a] (or (handle-client-message eid message a) a))))
        (recur))
      (on-client-disconnected eid))))


(defn add-new-client-connection [req]
  (let [eid (next-entity-id)
        ws-channel (:ws-channel req)
        a (create-avatar eid req)]
    (dosync
     (alter avatars assoc eid a)
     (send a (fn [s]
               (assoc s :ws-chan-reader
                      (spawn-wschan-reading-loop eid (:ws-channel s))))))))


;; == Handle client commands.


(defn on-client-disconnected [eid]
  (timbre/info "player" eid "disconnected")
  (dosync
   (game-remove-player eid)
   (alter avatars dissoc eid)))


(defmulti handle-client-message
  (fn [eid message a] (:command message)))


(defmethod handle-client-message :default [pid m a]
  (timbre/warn "unknown message from" pid ":" m)
  a)


(defmethod handle-client-message :join-game [pid m a]
  (timbre/info "new player" pid)
  (game-add-new-player pid)
  a)


(defmethod handle-client-message :user-input [pid m a]
  (timbre/trace "user input player" m)
  (game-process-user-input pid (:data m))
  (assoc a :user-input (:data m)))


;; == Send world patch

(declare take-world-snapshot)
(declare construct-world-patch)

(defn- map-val
  ([f] (map (fn [[k v]] [k (f v)])))
  ([f m] (into (empty m) (map-val f) m)))


(defn- create-and-send-world-patch [a w]
  (let [pid (:pid a)
        at (get-world-time w)]
    (if (w pid :player)
      (let [[a snapshot] (take-world-snapshot a w)
            [a wpatch] (construct-world-patch a snapshot)]
        (timbre/trace "send to" pid "wpatch" wpatch)
        (send-message-to-client
         pid
         {:command :wpatch
          :self pid
          :time at
          :ecs wpatch})
        (assoc a :actual-world w))
      (do
        (timbre/trace "player" pid "not in game" a (entity w pid))
        a))))


(defn send-snapshots-to-all-clients []
  (let [w (fix-world-state)]
    (dosync
     (doseq [[pid a] @avatars]
       (send-off a create-and-send-world-patch w)))))


(defn- econcat [& cols]
  (eduction cat cols))


(defn- make-component-patch [prev-cm cm]
  (into
   []
   cat
   [(eduction  ;; added & updated comps
     (filter (fn [[eid v]] (not= v (get prev-cm eid))))
     cm)
    (eduction  ;; removed comps
     (comp
      (filter (fn [[eid _]] (nil? (get cm eid))))
      (map (fn [[eid _]] [eid nil])))
     prev-cm)]))


(defn- construct-world-patch [a snapshot]
  (let [last-snapshot (:last-snapshot a)]
    [(assoc a :last-snapshot snapshot)
     (into {} (map (fn [[cid cm]]
                     [cid,
                      (make-component-patch
                       (get last-snapshot cid) cm)]))
           snapshot)]))


;; == World snapshot

(defn- point->vec [p]
  (when p
    ((juxt :x :y) p)))


(defn emap [f c]
  (eduction (map f) c))


(defn convey-component
  [w & {:keys [cid eids map-fn filter-fn]}]
  (let [ef (map (fn [eid] [eid (w eid cid)]))
        ef (if filter-fn (comp ef (filter filter-fn)) ef)
        ef (if map-fn (comp ef (map-val map-fn)) ef)
        ees (entity-ids w cid)
        eids (if eids (eids* eids ees) ees)]
    (into (im/int-map) ef eids)))


;; TODO: Game logic => move outside this ns.
(defn take-world-snapshot [a w]
  (let [pid (:pid a)
        visible0 (:visible-eids a)
        player-xy (w pid :position)
        is-visible? (fn [eid] (< (dist (w eid :position) player-xy) 1000))
        visible (into (im/dense-int-set) (filter is-visible?) (entity-ids w :position))
        cc (partial convey-component w)
        ]
    [(assoc a :visible-eids visible),
     {:type (cc :cid :type, :eids visible)
      :position (cc :cid :position, :map-fn point->vec, :eids visible)
      :angle (cc :cid :angle, :eids visible)
      :texture (cc :cid :texture, :eids visible)
      :dangle (cc :cid :dangle, :eids visible)
      :sprite (cc :cid :sprite, :eids visible)
      :score (cc :cid :score)
      :deaths (cc :cid :deaths)
      :self-player {pid true}
      :assets (cc :cid :assets)
      }]))
