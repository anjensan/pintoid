(ns pintoid.ecs.core
  (:use [pintoid.ecs.util]))

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

(defn update-comp
  ([w e c f] (put-comp w e c (f (get-comp w e c))))
  ([w e c f a1] (put-comp w e c (f (get-comp w e c) a1)))
  ([w e c f a1 a2] (put-comp w e c (f (get-comp w e c) a1 a2)))
  ([w e c f a1 a2 a3] (put-comp w e c (f (get-comp w e c) a1 a2 a3)))
  ([w e c f a1 a2 a3 & rs] (put-comp w e c (apply f (get-comp w e c) a1 a2 a3 rs))))

(defn update-comp!
  ([w e c f] (put-comp! w e c (f (get-comp w e c))))
  ([w e c f a1] (put-comp! w e c (f (get-comp w e c) a1)))
  ([w e c f a1 a2] (put-comp! w e c (f (get-comp w e c) a1 a2)))
  ([w e c f a1 a2 a3] (put-comp! w e c (f (get-comp w e c) a1 a2 a3)))
  ([w e c f a1 a2 a3 & rs] (put-comp! w e c (apply f (get-comp w e c) a1 a2 a3 rs))))

(defn has-entity? [w e]
  (not (nil? (get-entity-comps w e))))

(defn- assoc-entity-with-macro [f w e cvs]
  {:pre [(-> cvs count even?)
         (every? keyword? (take-nth 2 cvs))]}
  (let [es (gensym)]
    `(let [~es ~e]
       (-> ~w ~@(for [[c v] (partition 2 cvs)] (list f es c v))))))

(defmacro assoc-entity [w e & cvs]
  (assoc-entity-with-macro `put-comp w e cvs))

(defmacro assoc-entity! [w e & cvs]
  (assoc-entity-with-macro `put-comp! w e cvs))

(defmacro let-entity
  {:style/indent 3}
  [w e bs & body]
  (let [ws (gensym "w__")
        es (gensym "eid__")
        gc (fn [c] (if (seqable? c) `(get-comp ~ws ~es ~@c) `(get-comp ~ws ~es ~c)))
        bb (mapcat (fn [[s c]] [s (gc c)]) (partition-all 2 bs))]
    `(let [~ws ~w, ~es ~e] (when-some-ex [~@bb] ~@body))))

(defmacro each-entity
  {:style/indent 3}
  [w e bv & body]
  (let [ws (gensym "w__")
        es (gensym "eid__")
        bvp (partition-all 2 bv)
        csym (zipmap (map second bvp) (repeatedly #(gensym "cm__")))
        must (vec (filter keyword? (map second bvp)))
        emit-gcm (fn [[_ c]]
                   [(csym c)
                    (if (vector? c)
                      `(get-comp-map ~ws ~(first c))
                      `(get-comp-map ~ws ~c))])
        emit-gc (fn [[s c]]
                  [s (if (vector? c)
                       `(get ~(csym c) ~es ~(second c))
                       `(get ~(csym c) ~es))])]
    `(let [~ws ~w
           ~@(mapcat emit-gcm bvp)
           mc# (min-key count ~@(map csym must))]
       (eduction
        (comp
         (map (fn [~es]
                (let [~e ~es]
                  (when-some-ex [~@(mapcat emit-gc bvp)]
                    ~@body))))
         (filter some?))
        (keys mc#)))))

(defn get-full-entity [w e]
  (when-let [cids (get-entity-comps w e)]
    (zipmap cids (map #(get-comp w e %) cids))))

(defn entities
  ([w c]
   (keys (get-comp-map w c)))
  ([w c & cs]
   (let [[mc & rcs] (sort-by #(count (get-comp-map w %)) (cons c cs))]
     (eduction
      (reduce comp (map #(filter (get-comp-map w %)) rcs))
      (entities w mc)))))
