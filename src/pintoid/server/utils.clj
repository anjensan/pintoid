(ns pintoid.server.utils)

(def enable-debug false)


(defmacro log-info [& ms]
  `(println ~@ms))


(defmacro log-debug [& ms]
  (when enable-debug
    `(println ~@ms)))

