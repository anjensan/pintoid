(ns pintoid.server.cswiring
  (:use
   [pintoid.server game utils ecs math])
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


(declare handle-client-message)
(declare on-client-disconnected)

(defn- spawn-wschan-reading-loop [eid ws-channel]
  (go-loop []
    (if-let [{:keys [message error]} (<! ws-channel)]
      (do
        (when error
          (timbre/warnf "Client %s: %s" eid error))
        (when message
          (timbre/tracef "Receive from %s: %s" eid message)
          (send
           (get @avatars eid)
           (fn [a] (or (handle-client-message eid message a) a))))
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


;; == Handle client commands.

(defn on-client-disconnected [eid]
  (timbre/infof "Player %s disconnected" eid)
  (game-remove-player eid)
  (destroy-avatar (get @avatars eid))
  (swap! avatars dissoc eid))


(defmulti handle-client-message
  (fn [eid message a] (:command message)))


(defmethod handle-client-message :default [pid m a]
  (timbre/warnf "Unknown message from %s: m" pid m)
  a)


(defmethod handle-client-message :join-game [pid m a]
  (timbre/infof "New player %s" pid)
  (game-add-new-player pid)
  a)


(defmethod handle-client-message :user-input [pid m a]
  (timbre/trace "user input player" m)
  (game-process-user-input pid (:data m))
  (assoc a :user-input (:data m)))


;; == Send world patch

(declare make-world-dumper)

(defn- create-and-send-world-patch [a w]
  (let [pid (:pid a)
        at (get-world-time w)]
    (if (w pid :player)
      (let [ss (:ss-state a)
            wd (make-world-dumper pid w)
            [ss' dump] (wd ss w)
            wpatch (into {} (map (fn [[c d]] [c (vec d)])) dump)]
        (timbre/trace "send to" pid "wpatch" wpatch)
        (send-to-client
         pid
         {:command :wpatch
          :self pid
          :time at
          :ecs wpatch})
        (assoc a :actual-world w :ss-state ss'))
      (do
        (timbre/trace "player" pid "not in game" a (entity w pid))
        a))))


(defn send-snapshots-to-all-clients []
  (let [w (fix-world-state)]
    (dosync
     (doseq [[pid a] @avatars]
       (send-off a create-and-send-world-patch w)))))


;; == World snapshot

(defn- point->vec [p]
  (when p
    ((juxt :x :y) p)))


(defn emap [f c]
  (eduction (map f) c))


(defn econcat [c1 c2]
  (eduction cat [c1 c2]))


(defn merge-dumps2 [d1 d2]
  (merge-with #(eduction cat [%1 %2]) d1 d2))


(defn combine-dumpers* [dumpers]
  (fn [state w]
    (let [sds (map
               (fn [[c df]]
                 (let [s (get state c), [s' d] (df s w)] [c s' d]))
               dumpers)
          states (into {} (map (fn [[c s d]] [c s])) sds)
          dumps (reduce merge-dumps2 (map (fn [[c s d]] d) sds))
          ]
    [states dumps])))


(defmacro combine-dumpers [& dumpers]
  `(combine-dumpers*
    ~(zipmap (repeatedly #(keyword (gensym "ecs_dumper"))) dumpers)))


(defn map-val
  ([f] (map (fn [[k v]] [k (f v)])))
  ([f m] (into (empty m) (map-val f) m)))


(defn cdump [& {cid :cid eids :eids mapf :map fkey :fkey fval :fval}]
  (fn [{cm' :compmap d' :dump :or {d' {}, cm' {}} :as state} w]
    (let [cm (component-map w cid)]
      (if (= cm' cm)
        [state {}]
        (let [changed? (fn [[eid s]] (not (when-let [s' (d' eid)] (= s s'))))
              ef (cond-> identity
                   fkey (comp (filter fkey))
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
                           (and fkey (not (fkey eid)))
                           (and fval (not (fval (find cm eid)))))))
              d-rem (into [] (comp (map key) (filter removed?)) d')
              d (as-> d' $ (apply dissoc $ d-rem) (into $ d-add))]
          [(assoc state :compmap cm :dump d)
           {cid (econcat
                 (eduction (map (fn [x] [x nil])) d-rem)
                 (if mapf (eduction (map-val mapf) d-add) d-add))}])))))

;; == Dumpers
;; TODO: Game logic => move outside this ns.

(defn dumper-only-visible-entities [pid w]
  (let [player-xy (w pid :position)
        is-visible? #(when-let [f (w % :visible?)] (f w pid %))
        visible (into (im/dense-int-set) (filter is-visible?) (entity-ids w :position))]
    (combine-dumpers
     (cdump :cid :type, :eids visible)
     (cdump :cid :position, :map point->vec, :eids visible)
     (cdump :cid :position-tts, :eids visible)
     (cdump :cid :angle, :eids visible)
     (cdump :cid :layer, :eids visible)
     (cdump :cid :sprite, :eids visible))))


(defn dumper-self-player [pid]
  (fn [sent w]
    (if sent
      [true nil]
      [true {:self-player [[pid true]]}])))


(defn make-world-dumper [pid w]
  (combine-dumpers
   (dumper-only-visible-entities pid w)
    (cdump :cid :score)
    (cdump :cid :assets)
    (dumper-self-player pid)
    ))
