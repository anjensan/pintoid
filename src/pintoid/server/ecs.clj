(ns pintoid.server.ecs
  (:require [clojure.data.int-map :as im]))

;; -- API
(declare create-ecs)
(declare next-entity-id)
(declare add-new-entity)
(declare entity)

(declare make-timed-system)
(declare make-async-system)
(declare make-stateful-system)
(declare quantize-time)

(defprotocol ImmutableECS
  ;; (ecs 1 :x) ~ (component ecs 1 :x)
  (component [ecs entity-id component-id])
  (component-map [ecs component-id])
  (component-ids [ecs entity-id])
  )

(defprotocol PersistentECS
  ;; (conj ecs [1 :x ()]) ~ (put-component ecs 1 :x ())
  (put-component [ecs entity-id component-id comp])
  (add-entity [ecs entity-id] [ecs entity-id cid-comp-map])
  (drop-entity [ecs entity-id])
  )

(defprotocol TransientECS
  ;; (conj! ecs [1 :x ()]) ~ (put-component! ecs 1 :x ())
  (put-component! [ecs component-id entity-id comp]))


(def ^:private entity-id-counter (atom 16r400))

(defn next-entity-id
  ([] (next-entity-id :unknown))
  ([entity-kind] (swap! entity-id-counter inc)))


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

(defn make-timed-system
  ([sys-state-fn]
   (make-timed-system nil sys-state-fn))
  ([dt-quant sys-state-fn]
   (let [sid (next-entity-id)
         tq-fn (cond
                 (nil? dt-quant) (quantize-time)
                 (number? dt-quant) (quantize-time dt-quant)
                 (vector? dt-quant) (apply quantize-time dt-quant)
                 (fn? dt-quant) dt-quant)]
     (fn [ecs cur-time & rs]
       (let [prev-time (or (:last-time (ecs sid :system)) cur-time)
             dt (if prev-time (- cur-time prev-time) 0)
             dt-s (tq-fn ecs dt)
             target-time (reduce + prev-time dt-s)]
         (-> (if (== cur-time prev-time)
               ecs
               (reduce #(apply sys-state-fn %1 %2 rs) ecs dt-s))
             (add-entity sid {:system {:last-time target-time}
                              :type :system})))))))


(defn make-async-system
  ([system-fn-fn]
   (let [sid (next-entity-id :system)]
     [(fn [ecs & rs]
        (let [d (future (apply system-fn-fn ecs rs))]
          (-> ecs
              (add-entity sid {:system {:update-fn d}
                               :type :system}))))
      (fn [ecs & rs]
        (if-let [p (:update-fn (ecs sid :system))]
          (-> ecs
              (drop-entity sid)
              (apply @p ecs rs))))])))


(defn make-stateful-system
  ([sys-state-fn]
   (make-stateful-system nil sys-state-fn))
  ([initial-state sys-state-fn]
   (let [sid (next-entity-id :system)]
     (fn [ecs & rs]
       (let [state (ecs sid :system initial-state)
             [ecs' state'] (apply sys-state-fn ecs state)]
         (add-entity ecs' sid {:system state'
                               :type :system}))))))

;; -- misc

(defn create-ecs
  ([]
   (->PersistentECSImpl {} {} nil))
  ([eid-cid-comp-list]
   (into
    (reduce
     #(add-entity %1 %2)
     (create-ecs)
     (map first eid-cid-comp-list))
    eid-cid-comp-list)))

(defn drop-component [ecs entity-id component-id]
  (put-component ecs entity-id component-id nil))

(defn drop-component! [ecs entity-id component-id]
  (put-component! ecs entity-id component-id nil))

(defn grab-entity [ecs entity-id]
  (when-some [cids (component-ids ecs entity-id)]
    (into {} (map (fn [ck] [ck (ecs entity-id ck)]) cids))))

(defn add-new-entity [w cid-comp-map]
  (add-entity w (next-entity-id (:type cid-comp-map)) cid-comp-map))

(defn eids
  ([]
   (im/dense-int-set))
  ([xs]
   (if (instance? clojure.data.int_map.PersistentIntSet xs)
     xs
     (im/dense-int-set (seq xs)))))

(defn entities [w & cs]
  (let [[mc & rcs] (sort-by #(count (component-map w %)) cs)]
    ;; TODO: Use 'deftype + reduce-kv' instead of 'eduction'
    (eduction
     (reduce comp (map key) (map #(filter (component-map w %)) rcs))
     (component-map w mc))))

;; -- implementation

(deftype PersistentECSImpl
    [cid-eid-comp
     eid-cids
     the-meta]

  ImmutableECS

  (component [_ entity-id component-id]
    (when-some [cm (cid-eid-comp component-id)]
      (cm entity-id)))

  (component-map [_ component-id]
    (cid-eid-comp component-id))

  (component-ids [_ entity-id]
    (eid-cids entity-id))

  PersistentECS

  (drop-entity [this entity-id]
    (if-let [cids (eid-cids entity-id)]
      (PersistentECSImpl.
       (persistent!
        (reduce
         #(assoc! %1 %2 (dissoc (%1 %2) entity-id))
         (transient cid-eid-comp)
         cids))
       (dissoc eid-cids entity-id)
       the-meta)
      this))

  (add-entity [this entity-id]
    (if (eid-cids entity-id)
      this
      (PersistentECSImpl.
       cid-eid-comp
       (assoc eid-cids entity-id (or (eid-cids entity-id) (sorted-set)))
       the-meta)))

  (add-entity [this entity-id cid-comp-map]
    (PersistentECSImpl.
     (persistent!
      (reduce
       (fn [cec [k v]] (assoc! cec k (assoc (cec k) entity-id v)))
       (transient cid-eid-comp)
       cid-comp-map))
     (assoc eid-cids entity-id
            (into (or (eid-cids entity-id) (sorted-set)) (map first cid-comp-map)))
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
         eid-cids
         the-meta)

        ;; delete
        (and (not old-is-nil) new-is-nil)
        (PersistentECSImpl.
         (assoc-dissoc-coll cid-eid-comp component-id (dissoc eid-comp entity-id))
         (assoc eid-cids entity-id (disj (eid-cids entity-id) component-id))
         the-meta)

        ;; insert
        (and old-is-nil (not new-is-nil) (eid-cids entity-id))
        (PersistentECSImpl.
         (assoc cid-eid-comp component-id (assoc (or eid-comp {}) entity-id comp))
         (assoc eid-cids entity-id (conj (eid-cids entity-id) component-id))
         the-meta)

        :else this)))

  clojure.lang.Counted
  (count [_]
    (reduce + (map (comp count second) cid-eid-comp)))

  clojure.lang.IPersistentCollection

  (cons [this [entity-id component-id comp]]
    (.put-component this entity-id component-id comp))

  (empty [_]
    (PersistentECSImpl. {} {} the-meta))

  (seq [this]
    (seq
     (for [eid (keys eid-cids)
           cid (eid-cids eid)]
       [eid cid (this eid cid)])))

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
    (let [r (.component this entity-id component-id)]
      (if (nil? r) default r)))

  clojure.lang.IObj

  (meta [_]
    the-meta)

  (withMeta [_ meta]
    (PersistentECSImpl. cid-eid-comp eid-cids meta))

  clojure.lang.IEditableCollection

  (asTransient [_]
    (->TransientECSImpl
     (transient cid-eid-comp)
     eid-cids))

  )


(deftype TransientECSImpl
    [^:unsynchronized-mutable cid-eid-comp
     ^:unsynchronized-mutable eid-cids]

  ImmutableECS

  (component [_ entity-id component-id]
    (when-some [cm (cid-eid-comp component-id)]
      (cm entity-id)))

  (component-ids [_ entity-id]
    (eid-cids entity-id))

  TransientECS

  (put-component! [this entity-id component-id comp]
    (let [eid-comp (cid-eid-comp component-id)
          old-is-nil (nil? (when eid-comp (eid-comp entity-id)))
          new-is-nil (nil? comp)]
      (if-not new-is-nil

        (when (or (not old-is-nil) (eid-cids entity-id))
          ;; insert or update
          (when old-is-nil
            (set! eid-cids
                  (assoc!
                   (maybe-transient eid-cids) entity-id
                   (conj (or (eid-cids entity-id) (sorted-set)) component-id))))
          (let [new-eid-comp (assoc! (maybe-transient (or eid-comp {})) entity-id comp)]
            (when-not (identical? eid-comp new-eid-comp)
              (set! cid-eid-comp (assoc! cid-eid-comp component-id new-eid-comp)))))

        (when-not old-is-nil
          ;; delete
          (set! cid-eid-comp
                (assoc-dissoc-coll!
                 cid-eid-comp component-id
                 (dissoc! (maybe-transient eid-comp) entity-id)))
          (set! eid-cids
                (assoc!
                 (maybe-transient eid-cids) entity-id
                 (disj (eid-cids entity-id) component-id)))
          )))
    this)

  clojure.lang.ITransientCollection

  (conj [this [entity-id component-id comp]]
    (.put-component! this entity-id component-id comp))

  (persistent [_]
    (->PersistentECSImpl
     (persistent-map-rec1! cid-eid-comp)
     (maybe-persistent! eid-cids)
     nil))

  clojure.lang.IFn

  (invoke [this [entity-id component-id]]
    (.component this entity-id component-id))

  (invoke [this entity-id component-id]
    (.component this entity-id component-id))

  (invoke [this entity-id component-id default]
    (let [r (.component this entity-id component-id)]
      (if (nil? r) default r)))

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

(defn- convert-seq-to-integrals
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

(defn- quantize-time
  ([]
   (fn [ecs dt]
     [dt]))
  ([min-dt]
   (fn [ecs dt]
     (when (<= min-dt dt)
       [dt])))
  ([min-dt max-dt]
   (fn [ecs dt]
     (cond
       (> min-dt dt) []
       (<= min-dt dt max-dt) [dt]
       (zero? (rem dt max-dt)) (repeat (quot dt max-dt) max-dt)
       :else
       (let [n1 (int (/ dt max-dt))
             n2 (inc n1)
             n2dt (/ dt n2)]
         (if (<= min-dt n2dt max-dt)
           (convert-seq-to-integrals (repeat n2 n2dt))
           (repeat n1 max-dt)))))))
