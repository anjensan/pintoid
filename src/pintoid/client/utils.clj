(ns pintoid.client.utils)

(def enable-trace false)
(def enable-debug true)


(defmacro log [level & ms]
  (case level
    :trace (when enable-trace `(println ">>" ~@ms))
    :debug (when enable-debug `(println "dbg:" ~@ms))
    :info `(println "PINTOID:" ~@ms)
    `(println "???:" ~@ms)))


(defmacro log-info [& ms]
  `(log :info ~@ms))


(defmacro log-debug [& ms]
  `(log :debug ~@ms))


(defmacro log-trace [& ms]
  `(log :trace ~@ms))
