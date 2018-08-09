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

(defn assoc-dissoc-coll [c k v]
  (if (zero? (count v))
    (dissoc c k)
    (assoc c k v)))

(defn assoc-dissoc-coll! [c k v]
  (if (zero? (count v))
    (dissoc! c k)
    (assoc! c k v)))
