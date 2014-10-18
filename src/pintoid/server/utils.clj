(ns pintoid.server.utils)

(defmacro log-info [& ms]
  `(println ~@ms))


(defmacro log-debug [& ms]
  `(println ~@ms))
