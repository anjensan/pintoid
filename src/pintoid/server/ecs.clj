(ns pintoid.server.ecs
  (:require [clojure.data.int-map :as im])
  (:use [clojure.walk :only [postwalk]]))

;; -- API

(declare create-ecs)
(declare next-entity-id)
(declare add-new-entity)
(declare entity)

(declare system-timed)
(declare system-async)
(declare quantize-time)

;; sets of entity-ids
(declare eids+)
(declare eids*)
(declare eids-)
(declare eids$)
(declare eids)

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

(defn system-each-into [sys-fn eids-query]
  (fn [ecs & rs]
    (let [ss (eids$ ecs eids-query)]
      ;; workaround http://dev.clojure.org/jira/browse/DIMAP-2
      (if (seq (seq ss))
        (persistent!
         (reduce
          #(reduce conj! %1 (apply sys-fn ecs %2 rs))
          (transient ecs)
          ss))
        ecs))))

(defn system-timed
  ([sys-fn]
     (system-timed sys-fn (fn [dt] [dt])))
  ([sys-fn dt-quant-fn]
     (let [sid (next-entity-id)]
       (fn [ecs cur-time & rs]
         (let [prev-time (or (:last-time (ecs sid ::system)) cur-time)
               dt (if prev-time (- cur-time prev-time) 0)
               dt-s (dt-quant-fn dt)
               target-time (reduce + prev-time dt-s)]
           (-> (if (== cur-time prev-time)
                 ecs
                 (reduce #(apply sys-fn %1 %2 rs) ecs dt-s))
               (add-entity sid {::system {:last-time target-time :system-fn sys-fn}})))))))

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
                (add-entity sid {::system {:system-fn-fn system-fn-fn :update-fn d}}))))
        (fn [ecs & rs]
          (if-let [p (:update-fn (ecs sid ::system))]
            (-> ecs
                (drop-entity sid)
                (apply @p ecs rs))))])))

(defn system-stateful
  ([sys-fn]
     (system-stateful sys-fn nil))
  ([sys-fn initial-state]
     (let [sid (next-entity-id)]
       (fn [ecs & rs]
         (let [state (:state (ecs sid ::system) initial-state)
               [ecs' state'] (apply sys-fn ecs state)]
           (-> ecs'
               (add-entity sid {::system {:system-fn sys-fn :state state'}})))))))

;; -- misc

(defn create-ecs
  ([]
     (->PersistentECSImpl {} {} {} nil))
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

(defn entity [ecs entity-id]
  (when-let [cids (component-ids ecs entity-id)]
    (into {} (map (fn [ck] [ck (ecs entity-id ck)]) cids))))

(defn add-new-entity [w cid-comp-map]
  (add-entity w (next-entity-id) cid-comp-map))

(defn eids+ [& es]
  (reduce im/union (map eids es)))

(defn eids* [& es]
  (reduce im/intersection (map eids es)))

(defn eids- [x & es]
  (reduce im/difference (eids x) (map eids es)))

(defn eids
  ([]
     (im/dense-int-set))
  ([xs]
     (if (instance? clojure.data.int_map.PersistentIntSet xs)
       xs
       (im/dense-int-set (seq xs)))))

(defn eids$ [ces eids-query]
  (cond
   (keyword? eids-query) (entity-ids ces eids-query)
   (empty? eids-query) (eids)
   (vector? eids-query)
   (let [[f & r :as fr] eids-query]
     (condp #(%1 %2) f
       #{:* '*} (apply eids* (map #(eids$ ces %) r))
       #{:+ '+} (apply eids+ (map #(eids$ ces %) r))
       #{:- '-} (apply eids- (map #(eids$ ces %) r))
       (eids fr)))
   :else (eids eids-query)))

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

  (drop-entity [this entity-id]
    (if-let [cids (eid-cids entity-id)]
      (PersistentECSImpl.
       (persistent!
        (reduce
         #(assoc! %1 %2 (dissoc (%1 %2) entity-id))
         (transient cid-eid-comp)
         cids))
       (persistent!
        (reduce
         #(assoc! %1 %2 (disj (%1 %2) entity-id))
         (transient cid-eids)
         cids))
       (dissoc eid-cids entity-id)
       the-meta)
      this))

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
     (persistent!
      (reduce
       (fn [cec [k v]] (assoc! cec k (assoc (cec k) entity-id v)))
       (transient cid-eid-comp)
       cid-comp-map))
     (persistent!
      (reduce
       #(assoc! %1 %2 (conj (or (%1 %2) (eids)) entity-id))
       (transient cid-eids)
       (map first cid-comp-map)))
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
        (assoc cid-eids component-id (conj (or (cid-eids component-id) (eids)) entity-id))
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
    (PersistentECSImpl. {} {} {} the-meta))

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
    (PersistentECSImpl. cid-eid-comp cid-eids eid-cids meta))

  clojure.lang.IEditableCollection

  (asTransient [_]
    (->TransientECSImpl
     (transient cid-eid-comp)
     cid-eids
     eid-cids))

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
    (cid-eids component-id))

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
                   (conj (or (eid-cids entity-id) (sorted-set)) component-id)))
            (set! cid-eids
                  (assoc!
                   (maybe-transient cid-eids) component-id
                   (conj (or (cid-eids component-id) (eids)) entity-id))))

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
          )))
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
