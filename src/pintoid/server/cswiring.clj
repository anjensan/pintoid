(ns pintoid.server.cswiring
  (:use
   pintoid.server.game.core
   [pintoid.server utils ecs math])
  (:require
   [mount.core :refer [defstate]]
   [taoensso.timbre :as timbre]
   [clojure.data.int-map :as im]
   [clojure.core.async :refer
    [<! >! <!! >!! put! close! thread go chan go-loop]]
   [cheshire.core :as json]
   [clojure.set :refer [union]]))


(declare destroy-avatar)

(defstate avatars
  :start (atom {})
  :stop (run! destroy-avatar (vals @avatars)))


(defn create-avatar [pid req]
  (timbre/tracef "Create avatar for pid %s, req %s" pid req)
  (agent
   {:pid pid
    :host (:remote-addr req)
    :ws-channel (:ws-channel req)}))

(defn destroy-avatar [avatar]
  (timbre/trace "Send destroy function to avatar" avatar)
  (send
   avatar
   (fn [as]
     (timbre/debug "Destroy avatar" (:pid as))
     (close! (:ws-channel as))
     ::destroyed)))

(defn send-to-client [pid message]
  (timbre/tracef "Send to %s: %s" pid message)
  (send
   (get @avatars pid)
   (fn [avatar]
     (timbre/trace "pid" pid ">>" message)
     (go (>! (:ws-channel avatar) message))
     avatar)))


(defmulti handle-client-message
  (fn [a eid message] (:command message)))

(defmethod handle-client-message :default [a pid m]
  (timbre/warnf "Unknown message from %s: m" pid m)
  a)

(defmethod handle-client-message :join-game [a pid m]
  (timbre/infof "New player %s" pid)
  (game-add-new-player pid)
  a)

(defmethod handle-client-message :user-input [a pid m]
  (timbre/trace "user input player" m)
  (game-process-user-input pid (:data m))
  (assoc a :user-input (:data m)))


(defn on-client-disconnected [eid]
  (timbre/infof "Player %s disconnected" eid)
  (game-remove-player eid)
  (destroy-avatar (get @avatars eid))
  (swap! avatars dissoc eid))


(defn- spawn-wschan-reading-loop [eid ws-channel]
  (go-loop []
    (if-let [{:keys [message error]} (<! ws-channel)]
      (do
        (when error
          (timbre/warnf "Client %s: %s" eid error))
        (when message
          (timbre/tracef "Receive from %s: %s" eid message)
          (send (get @avatars eid) handle-client-message eid message))
        (recur))
      (on-client-disconnected eid))))

(defn new-client-connection [req]
  (let [eid (next-entity-id)
        ws-channel (:ws-channel req)
        avatar (create-avatar eid req)]
    (swap! avatars assoc eid avatar)
    (send
     avatar
     (fn [s]
       (assoc s :ws-chan-reader
              (spawn-wschan-reading-loop eid (:ws-channel s)))))))


;; == Send world patch

(declare make-world-dumper)

(defn- create-and-send-world-patch [a at w]
  (let [pid (:pid a)]
    (if (w pid :player)
      (let [ss (:ss-state a)
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
               :ss-state ss'))
      (do
        (timbre/tracef "player %s not in game" (entity w pid))
        a))))

(defn send-snapshots-to-all-clients []
  (let [[at w] (get-world)]
    (dosync
     (doseq [[pid a] @avatars]
       (send-off a create-and-send-world-patch at w)))))


;; == Dumpers

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
