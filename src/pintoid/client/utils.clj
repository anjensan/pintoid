(ns pintoid.client.utils)

(defmacro log-info [& ms]
  `(println "PINTOID:" ~@ms))


(defmacro log-debug [& ms]
  `(println ">>" ~@ms))
