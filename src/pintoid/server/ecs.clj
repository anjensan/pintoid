(ns pintoid.server.ecs
  (:require [clojure.data.int-map :as im]))

;; -- API

(declare create-ecs)
(declare next-entity-id)
(declare entity)

(declare system-timed)
(declare system-async)
(declare quantize-time)

;; sets of entity-ids
(declare eid-set+)
(declare eid-set*)
(declare eid-set-)
(declare eid-set)

(defprotocol ImmutableECS
  ;; (ecs 1 :x) ~ (component ecs 1 :x)
  (component [ecs entity-id component-id])
  (component-ids [ecs entity-id])
  (entity-ids [ecs component-id])
  )

(defprotocol PersistentECS
  ;; (conj ecs [1 :x ()]) ~ (put-component ecs 1 :x ())
  (put-component [ecs component-id entity-id comp])
  (add-entity [ecs entity-id] [ecs entity-id cid-comp-map])
  (drop-entity [ecs entity-id])
  )

(defprotocol TransientECS
  ;; (conj! ecs [1 :x ()]) ~ (put-component! ecs 1 :x ())
  (put-component! [ecs component-id entity-id comp]))


;; -- entity-ids

(def ^:private entity-id-counter (atom 16r400))

(defn next-entity-id []
  (swap! entity-id-counter inc))


;; -- declarations

(declare maybe-transient)
(declare maybe-persistent!)
(declare persistent-map-rec1!)
(declare assoc-dissoc)
(declare assoc-dissoc-coll)
(declare assoc-dissoc-coll!)
(declare ->PersistentECSImpl)
(declare ->TransientECSImpl)


;; -- systems

(def ^:private systems-counter (atom 0))

(defn current-os-time []
  (System/currentTimeMillis))

(defn system-timed
  ([sys-fn]
     (system-timed sys-fn (fn [dt] [dt])))
  ([sys-fn dt-quant-fn]
     (system-timed sys-fn dt-quant-fn current-os-time))
  ([sys-fn dt-quant-fn current-time-fn!]
     (let [sid (next-entity-id)]
       (fn [ecs]
         (let [cur-time (current-time-fn!)
               prev-time (or (:last-time (ecs sid ::system-timed)) cur-time)
               dt (if prev-time (- cur-time prev-time) 0)
               dt-s (dt-quant-fn dt)
               target-time (reduce + prev-time dt-s)]
           (-> (if (== cur-time prev-time)
                 ecs
                 (reduce #(sys-fn %1 %2) ecs dt-s))
               (add-entity sid)
               (put-component sid ::system-timed
                              {:last-time target-time :system-fn sys-fn})))))))

(defn- convert-to-integrals
  [xs]
  (let [[nxs r]
        (reduce
         (fn [[s acc] x]
           (let [xx (+ x acc)
                 xxi (long xx)
                 xxd (- xx xxi)]
             (if (<= xxd 0.5)
               [(conj s xxi) xxd]
               [(conj s (inc xxi)) (- xxd 1)])))
         [[] 0]
         xs)]
    (when (seq nxs)
      (concat (butlast nxs) (vector (long (+ r (last nxs))))))))

(defn quantize-time
  ([min-dt]
     (fn [dt]
       (when (<= min-dt dt)
         [dt])))
  ([min-dt max-dt]
     (fn [dt]
       (cond
        (> min-dt dt) []
        (<= min-dt dt max-dt) [dt]
        (zero? (rem dt max-dt)) (repeat (quot dt max-dt) max-dt)
        :else
        (let [n1 (int (/ dt max-dt))
              n2 (inc n1)
              n2dt (/ dt n2)]
          (if (<= min-dt n2dt max-dt)
            (convert-to-integrals (repeat n2 n2dt))
            (repeat n1 max-dt)))))))

(defn system-async
  ([system-fn-fn]
     (let [sid (next-entity-id)]
       [(fn [ecs & rs]
          (let [d (future (apply system-fn-fn ecs rs))]
            (-> ecs
                (add-entity ecs sid)
                (put-component sid ::system-async
                               {:system-fn-fn system-fn-fn :update-fn d}))))
        (fn [ecs & rs]
          (if-let [p (:update-fn (ecs sid ::system-async))]
            (-> ecs
                (put-component sid ::system-async
                               {:system-fn-fn system-fn-fn :update-fn nil})
                (apply @p ecs rs))))])))

(defn system-stateful
  ([sys-fn]
     (system-stateful sys-fn nil))
  ([sys-fn initial-state]
     (let [sid (next-entity-id)]
       (fn [ecs & rs]
         (let [state (:state (ecs sid ::system-stateful) initial-state)
               [ecs' state'] (apply sys-fn ecs state rs)]
           (-> ecs'
               (add-entity sid)
               (put-component sid ::system-stateful
                              {:system-fn sys-fn :state state'})))))))

;; -- misc

(defn create-ecs
  ([]
     (->PersistentECSImpl {} {} {} nil))
  ([cid-eid-comp-list]
     (into
      (reduce #(add-entity %1 %2)
              (create-ecs)
              (map second cid-eid-comp-list))
      cid-eid-comp-list)))

(defn drop-component [ecs entity-id component-id]
  (put-component ecs entity-id component-id nil))

(defn drop-component! [ecs entity-id component-id]
  (put-component! ecs entity-id component-id nil))

(defn entity [ecs entity-id]
  (when-let [cids (component-ids ecs entity-id)]
    (into {} (map (fn [ck] [ck (ecs ck entity-id)]) cids))))

(defn eid-set+ [& es]
  (reduce im/union es))

(defn eid-set* [& es]
  (reduce im/intersection es))

(defn eid-set- [x & es]
  (reduce im/difference x es))

(defn eid-set
  ([]
     (im/dense-int-set))
  ([xs]
     (im/dense-int-set xs)))


;; -- implementation

(deftype PersistentECSImpl
    [cid-eid-comp
     cid-eids
     eid-cids
     the-meta]

  ImmutableECS

  (component [_ entity-id component-id]
    (when-let [cm (cid-eid-comp component-id)]
      (cm entity-id)))

  (component-ids [_ entity-id]
    (eid-cids entity-id))

  (entity-ids [_ component-id]
    (cid-eids component-id))

  PersistentECS

  (add-entity [this entity-id]
    (if (eid-cids entity-id)
      this
      (PersistentECSImpl.
       cid-eid-comp
       cid-eids
       (assoc eid-cids entity-id (or (eid-cids entity-id) (sorted-set)))
       the-meta)))

  (add-entity [this entity-id cid-comp-map]
    (PersistentECSImpl.
     (reduce (fn [cec [k v]] (assoc cec k (assoc (cec k) entity-id v))) cid-eid-comp cid-comp-map)
     (reduce #(assoc %1 %2 (conj (or (%1 %2) (eid-set)) entity-id)) cid-eids (map first cid-comp-map))
     (assoc eid-cids entity-id (into (or (eid-cids entity-id) (sorted-set)) (map first cid-comp-map)))
     the-meta))

  (put-component [this entity-id component-id comp]
    (let [eid-comp (cid-eid-comp component-id)
          old-is-nil (nil? (when eid-comp (eid-comp entity-id)))
          new-is-nil (nil? comp)]
      (cond

       ;; update
       (not (or old-is-nil new-is-nil))
       (PersistentECSImpl.
        (assoc cid-eid-comp component-id (assoc eid-comp entity-id comp))
        cid-eids
        eid-cids
        the-meta)

       ;; delete
       (and (not old-is-nil) new-is-nil)
       (PersistentECSImpl.
        (assoc-dissoc-coll cid-eid-comp component-id (dissoc eid-comp entity-id))
        (assoc cid-eids component-id (disj (cid-eids component-id) entity-id))
        (assoc eid-cids entity-id (disj (eid-cids entity-id) component-id))
        the-meta)

       ;; insert
       (and old-is-nil (not new-is-nil) (eid-cids entity-id))
       (PersistentECSImpl.
        (assoc cid-eid-comp component-id (assoc (or eid-comp {}) entity-id comp))
        (assoc cid-eids component-id (conj (or (cid-eids component-id) (eid-set)) entity-id))
        (assoc eid-cids entity-id (conj (eid-cids entity-id) component-id))
        the-meta)

       :else this)))

  clojure.lang.IPersistentCollection

  (count [_]
    (reduce + (map (comp count second) cid-eid-comp)))

  (cons [this [entity-id component-id comp]]
    (.put-component this entity-id component-id comp))

  (empty [_]
    (PersistentECSImpl. {} {} {} the-meta))

  (seq [this]
    (for [eid (keys eid-cids)
          cid (eid-cids eid)]
      [eid cid (this eid cid)]))

  (equiv [this other]
    (and
     (== (count this) (count other))
     (= (seq this) (seq other))))

  clojure.lang.IFn

  (invoke [this [entity-id component-id]]
    (.component this entity-id component-id))

  (invoke [this entity-id component-id]
    (.component this entity-id component-id))

  (invoke [this entity-id component-id default]
    (or (.component this entity-id component-id) default))

  clojure.lang.IObj

  (meta [_]
    the-meta)

  (withMeta [_ meta]
    (PersistentECSImpl. cid-eid-comp cid-eids eid-cids meta))

  clojure.lang.IEditableCollection

  (asTransient [_]
    (->TransientECSImpl
     (transient cid-eid-comp)
     cid-eids
     eid-cids
     ))
  )


(deftype TransientECSImpl
    [^:volatile-mutable cid-eid-comp
     ^:volatile-mutable cid-eids
     ^:volatile-mutable eid-cids]

  ImmutableECS

  (component [_ entity-id component-id]
    (when-let [cm (cid-eid-comp component-id)]
      (cm entity-id)))

  (component-ids [_ entity-id]
    (eid-cids entity-id))

  (entity-ids [_ component-id]
    (eid-cids component-id))

  TransientECS

  (put-component! [this entity-id component-id comp]
    (let [eid-comp (cid-eid-comp component-id)
          old-is-nil (nil? (when eid-comp (eid-comp entity-id)))
          new-is-nil (nil? comp)]
      (if-not new-is-nil
        (do
          (when old-is-nil
            ;; insert
            (set! eid-cids (assoc! (maybe-transient eid-cids)
                                   entity-id (conj (eid-cids entity-id) component-id)))
            (set! cid-eids (assoc! (maybe-transient cid-eids)
                                   component-id (conj (or (cid-eids component-id) (eid-set) entity-id))))
          ;; insert/update
          (let [new-eid-comp (assoc! (maybe-transient (or eid-comp {})) entity-id comp)]
            (when-not (identical? eid-comp new-eid-comp)
              (set! cid-eid-comp (assoc! cid-eid-comp component-id new-eid-comp)))))

        (when-not old-is-nil
          ;; delete
          (set! cid-eid-comp
                (assoc-dissoc-coll!
                 cid-eid-comp component-id
                 (dissoc! (maybe-transient eid-comp) entity-id)))
          (set! cid-eids
                (assoc!
                 (maybe-transient cid-eids) component-id
                 (disj (cid-eids component-id) entity-id)))
          (set! eid-cids
                (assoc!
                 (maybe-transient eid-cids) entity-id
                 (disj (eid-cids entity-id) component-id)))
          ))))
    this)

  clojure.lang.ITransientCollection

  (conj [this [entity-id component-id comp]]
    (.put-component! this entity-id component-id comp))

  (persistent [_]
    (->PersistentECSImpl
     (persistent-map-rec1! cid-eid-comp)
     (maybe-persistent! cid-eids)
     (maybe-persistent! eid-cids)
     nil))

  clojure.lang.IFn

  (invoke [this [entity-id component-id]]
    (.component this component-id entity-id))

  (invoke [this entity-id component-id]
    (.component this component-id entity-id))

  (invoke [this entity-id component-id default]
    (or (.component this entity-id component-id) default))

  )


;; private utils

(definline ^:private maybe-transient [c]
  `(let [c# ~c]
     (if (instance? clojure.lang.ITransientCollection c#)
       c#
       (transient c#))))

(definline ^:private maybe-persistent! [c]
  `(let [c# ~c]
     (if (instance? clojure.lang.ITransientCollection c#)
       (persistent! c#)
       c#)))

(defn- persistent-map-rec1! [tm]
  (if (instance? clojure.lang.ITransientCollection tm)
    (let [pm (persistent! tm)]
      (persistent!
       (reduce-kv
        #(if (instance? clojure.lang.ITransientCollection %3)
           (assoc! %1 %2 (persistent! %3))
           %1)
        (transient pm)
        pm)))
    tm))

(defn- assoc-dissoc-coll [c k v]
  (if (zero? (count v))
    (dissoc c k)
    (assoc c k v)))

(defn- assoc-dissoc-coll! [c k v]
  (if (zero? (count v))
    (dissoc! c k)
    (assoc! c k v)))
