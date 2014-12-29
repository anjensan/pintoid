(ns pintoid.client.utils)

(def enable-trace false)
(def enable-debug false)

(defmacro log [level & ms]
  (case level
    :trace (when enable-trace `(println ">>" ~@ms))
    :debug (when enable-debug `(println "dbg:" ~@ms))
    :info `(println "PINTOID:" ~@ms)
    `(println "???:" ~@ms)))
