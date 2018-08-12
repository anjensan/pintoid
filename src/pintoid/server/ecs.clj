(ns pintoid.server.ecs
  (:use [pintoid.server.utils])
  (:require [clojure.data.int-map :as im]))

(defprotocol ImmutableECS
  (get-comp [w e c] [w e c d])
  (get-comp-map [w c])
  (get-entity-comps [w e]))

(defprotocol PersistentECS
  (put-comp [w e c v])
  (add-entity [w e] [w e cvs])
  (remove-entity [w e]))

(defprotocol TransientECS
  (put-comp! [w c e v]))

(def ^:const empty-comp-map (im/int-map))
(def ^:const empty-comp-set (sorted-set))

(def ^:private entity-id-counter (atom 16r400))

(defn next-entity
  ([] (next-entity :unknown))
  ([entity-kind] (swap! entity-id-counter inc)))

(defn add-new-entity [w cvs]
  (add-entity w (next-entity (:type cvs)) cvs))

(defn drop-comp [w e c]
  (put-comp w e c nil))

(defn drop-comp! [w e c]
  (put-comp! w e c nil))

(defn- assoc-entity-with [f w e cvs]
  {:pre [(-> cvs count even?)
         (every? keyword? (take-nth 2 cvs))]}
  (reduce (fn [c v] (f w e c v)) w (partition 2 cvs)))

(defn assoc-entity [w e & cvs]
  (assoc-entity-with put-comp w e cvs))

(defn assoc-entity! [w e & cvs]
  (assoc-entity-with put-comp! w e cvs))

(defmacro let-entity [w e bs & body]
  (let [ws (gensym)
        es (gensym)
        gc (fn [c] (if (seqable? c) `(get-comp ~ws ~es ~@c) `(get-comp ~ws ~es ~c)))
        bb (mapcat (fn [[s c]] [s (gc c)]) (partition-all 2 bs))]
    `(let [~ws ~w, ~es ~e, ~@bb] ~@body)))

(defn get-full-entity [w e]
  (when-let [cids (get-entity-comps w e)]
    (zipmap cids (map #(get-comp w e %) cids))))

(defn entities
  ([w c]
   (eduction-map-key identity (get-comp-map w c)))
  ([w c & cs]
   (let [cs (cons c cs)
         [mc & rcs] (sort-by #(count (get-comp-map w %)) cs)]
     (eduction-map-key
      (if (seq cs)
        (reduce comp (map #(filter (get-comp-map w %)) rcs))
        identity)
      (get-comp-map w mc)))))

(declare ->PersistentECSImpl)
(declare ->TransientECSImpl)

(defn create-ecs
  ([] (->PersistentECSImpl {} {} nil))
  ([eid-cid-comp-list]
   (into
    (transduce (map first) add-entity (create-ecs) eid-cid-comp-list)
    eid-cid-comp-list)))

(deftype PersistentECSImpl [cev-m ec-m met]

  ImmutableECS
  (get-comp [_ e c]
    (when-let [cm (get cev-m c)]
      (get cm e)))
  (get-comp [_ e c d]
    (if-let [cm (get cev-m c)]
      (get cm e d)
      d))
  (get-comp-map [_ c]
    (get cev-m c))
  (get-entity-comps [_ e]
    (get ec-m e))

  PersistentECS
  (remove-entity [this e]
    (if-let [cids (get ec-m e)]
      (PersistentECSImpl.
       (persistent!
        (reduce
         (fn [cec k] (assoc! cec k (dissoc (get cev-m k) e)))
         (transient cev-m)
         cids))
       (dissoc ec-m e)
       met)
      this))

  (add-entity [this e]
    (if (get ec-m e)
      this
      (PersistentECSImpl.
       cev-m
       (assoc ec-m e empty-comp-set)
       met)))

  (add-entity [this e cvs]
    (PersistentECSImpl.
     (persistent!
      (reduce
       (fn [cec [k v]] (assoc! cec k (assoc (get cec k) e v)))
       (transient cev-m)
       cvs))
     (assoc ec-m e (into (get ec-m e empty-comp-set) (map first) cvs))
     met))

  (put-comp [this e c v]
    (let [cm (get cev-m c)
          old? (contains? cm e)
          new? (not (nil? v))]
      (cond
        (and old? new?) ;; update
        (PersistentECSImpl.
         (assoc cev-m c (assoc cm e v))
         ec-m
         met)

        (and old? (not new?)) ;; delete
        (PersistentECSImpl.
         (assoc-dissoc-coll cev-m c (dissoc cm e))
         (update ec-m e disj c)
         met)

        (and (not old?) new? (contains? ec-m e)) ;; insert
        (PersistentECSImpl.
         (assoc cev-m c (assoc (or cm empty-comp-map) e v))
         (update ec-m e conj c)
         met)

        :else this)))

  clojure.lang.Counted
  (count [_]
    (reduce + (map (comp count val) cev-m)))

  clojure.lang.IPersistentCollection
  (cons [this [e c v]]
    (.put-comp this e c v))
  (empty [_]
    (PersistentECSImpl. {} {} met))
  (seq [this]
    (seq
     (for [e (keys ec-m)
           c (ec-m e)]
       [e c (this e c)])))
  (equiv [this other]
    (and
     (== (count this) (count other))
     (= (seq this) (seq other))))

  clojure.lang.IFn
  (invoke [this e c]
    (.get-comp this e c))
  (invoke [this e c default]
    (let [r (.get-comp this e c)]
      (if (nil? r) default r)))

  clojure.lang.IObj
  (meta [_]
    met)
  (withMeta [_ meta]
    (PersistentECSImpl. cev-m ec-m meta))

  clojure.lang.IEditableCollection
  (asTransient [_]
    (->TransientECSImpl cev-m ec-m)))

(defn- persistent-map-rec1! [tm]
  (if-not (transient? tm)
    tm
    (let [pm (persistent! tm)]
      (persistent!
       (reduce-kv
        (fn [a k v] (if (transient? v) (assoc! a k (persistent! v)) a))
        (transient pm)
        pm)))))

(deftype TransientECSImpl [^:unsynchronized-mutable cev-m
                           ^:unsynchronized-mutable ec-m]
  ImmutableECS
  (get-comp [_ e c]
    (when-let [cm (get cev-m c)]
      (get cm e)))
  (get-comp [_ e c d]
    (if-let [cm (get cev-m c)]
      (get cm e d)
      d))
  (get-comp-map [_ c]
    (when-let [cm (get cev-m c)]
      (if-not (transient? cm)
        cm
        (let [pm (persistent! cm)]
          (set! cev-m (assoc! (maybe-transient cev-m) c pm))
          pm))))
  (get-entity-comps [_ e]
    (when-let [es (get ec-m e)]
      (if-not (transient? es)
        es
        (let [p (persistent! es)]
          (set! ec-m (assoc! (maybe-transient ec-m) e p))
          p))))

  TransientECS
  (put-comp! [this e c v]
    (let [cm (cev-m c)
          old? (contains? cm e)
          new? (not (nil? v))]
      (cond
        (and old? new?) ;; update
        (let [cm' (assoc! (maybe-transient (or cm empty-comp-map)) e v)]
          (when-not (identical? cm cm')
            (set! cev-m (assoc! (maybe-transient cev-m) c cm'))))

        (and old? (not new?)) ;; delete
        (do
          (set! cev-m (assoc-dissoc-coll! (maybe-transient cev-m) c (dissoc! (maybe-transient cm) e)))
          (set! ec-m (assoc! (maybe-transient ec-m) e (disj (get ec-m e) c))))

        (and (not old?) new? (contains? ec-m e)) ;; insert
        (do
          (let [cm' (assoc! (maybe-transient (or cm empty-comp-map)) e v)]
            (when-not (identical? cm cm')
              (set! cev-m (assoc! (maybe-transient cev-m) c cm'))))
          (set! ec-m (assoc! (maybe-transient ec-m) e (conj (get ec-m e empty-comp-set) c))))))
    this)

  clojure.lang.ITransientCollection
  (conj [this [e c v]]
    (.put-comp! this e c v))
  (persistent [_]
    (->PersistentECSImpl
     (persistent-map-rec1! cev-m)
     (maybe-persistent! ec-m)
     nil))

  clojure.lang.IFn
  (invoke [this e c]
    (.get-comp this e c))
  (invoke [this e c default]
    (let [r (.get-comp this e c)]
      (if (nil? r) default r)))
  )
