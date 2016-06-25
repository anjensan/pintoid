(ns pintoid.client.utils)

(def log-level :info)


(defmacro foreach! [[s sq & {xf :xf}] & body]
  `(reduce (~(or xf `identity) (fn [_# ~s] ~@body)) nil ~sq))


(defmacro log [level & ms]
  (case level
    :trace (when (#{:trace} log-level) `(println "TRACE:" ~@ms))
    :debug (when (#{:trace :debug} log-level) `(println "DEBUG: " ~@ms))
    :info `(println "PINTOID:" ~@ms)
    (throw (ex-info "Unknown log level" {:level level}))))
