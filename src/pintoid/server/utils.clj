(ns pintoid.server.utils)

(defmacro when-some* [bindings & body]
  (if (empty? bindings)
    (list* 'do body)
    (let [cb (subvec bindings 0 2)
          rb (subvec bindings 2)]
      `(when-some ~cb
         (when-some* ~rb ~@body)))))

(defn transient? [c]
  (instance? clojure.lang.ITransientCollection c))

(defn maybe-transient [c]
  (if (transient? c) c (transient c)))

(defn maybe-persistent! [c]
  (if (transient? c) (persistent! c) c))

(defn transient-reduce [f i c]
  (persistent! (reduce f i (transient c))))

(defn assoc-dissoc-coll [c k v]
  (if (zero? (count v))
    (dissoc c k)
    (assoc c k v)))

(defn assoc-dissoc-coll! [c k v]
  (if (zero? (count v))
    (dissoc! c k)
    (assoc! c k v)))

(deftype MapKeyEduction [xform coll]

  clojure.lang.Sequential
  java.lang.Iterable
  (iterator [_]
    (clojure.lang.TransformerIterator/create
     (comp (map key) xform)
     (clojure.lang.RT/iter coll)))

  clojure.lang.IReduceInit
  (reduce [_ f init]
    (let [f (xform (completing f))
          rf (fn [a k v] (f a k))
          ret (reduce-kv rf init coll)]
      (f ret))))

(defn eduction-map-key
  "Same as (eduction (comp (map key) ~xform) ~coll) but a bit faster."
  {:arglists '([xform* coll])}
  [& xforms]
  (MapKeyEduction. (apply comp (butlast xforms)) (last xforms)))
