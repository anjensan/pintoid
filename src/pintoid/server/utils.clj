(ns pintoid.server.utils)

(def enable-debug false)

(defn current-os-time []
  (System/currentTimeMillis))

(defmacro when-some* [bindings & body]
  (if (empty? bindings)
    (list* 'do body)
    (let [cb (subvec bindings 0 2)
          rb (subvec bindings 2)]
      `(when-some ~cb
         (when-some* ~rb ~@body)))))

(defmacro log-info [& ms]
  `(println ~@ms))

(defmacro log-debug [& ms]
  (when enable-debug
    `(println ~@ms)))

(defn current-os-time []
  (System/currentTimeMillis))
