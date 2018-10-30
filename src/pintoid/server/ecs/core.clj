(ns pintoid.server.ecs.core
  (:use [pintoid.server.ecs.util]))

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

(defn update-comp [w e c f & as]
  (put-comp w e c (apply f (get-comp w e c) as)))

(defn update-comp! [w e c f & as]
  (put-comp! w e c (apply f (get-comp w e c) as)))

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

(defmacro let-entity [w e bs & body]
  (let [ws (gensym)
        es (gensym)
        gc (fn [c] (if (seqable? c) `(get-comp ~ws ~es ~@c) `(get-comp ~ws ~es ~c)))
        bb (mapcat (fn [[s c]] [s (gc c)]) (partition-all 2 bs))]
    `(let [~ws ~w, ~es ~e, ~@bb] ~@body)))

(defn get-full-entity [w e]
  (when-let [cids (get-entity-comps w e)]
    (zipmap cids (map #(get-comp w e %) cids))))

(defn entities [w c]
   (let [cs (if (seqable? c) (seq c) (list c))
         [mc & rcs] (sort-by #(count (get-comp-map w %)) cs)]
     (eduction-map-key
      (if (seq cs)
        (reduce comp (map #(filter (get-comp-map w %)) rcs))
        identity)
      (get-comp-map w mc))))

(defn entities-reduce
  ([w c f]
   (entities-reduce w c identity f))
  ([w c xf f]
   (transduce xf
              (fn ([w'] w') ([w' e] (or (f w' e) w')))
              w
              (entities w c))))

(defn entities-reduce!
  ([w c f]
   (entities-reduce! w c identity f))
  ([w c xf f]
   (transduce xf
              (fn ([w'] (persistent! w')) ([w' e] (or (f w' e) w')))
              (transient w)
              (entities w c))))
