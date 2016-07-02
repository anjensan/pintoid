(ns pintoid.client.macros)

(def log-level :info)


(defmacro foreach! [[s sq & {xf :xf}] & body]
  `(reduce (~(or xf `identity) (fn [_# ~s] ~@body)) nil ~sq))


;; TODO: replace with something better (klang?)
(defmacro log [level & ms]
  (case level
    :trace (when (#{:trace} log-level) `(println "TRACE:" ~@ms))
    :debug (when (#{:trace :debug} log-level) `(println "DEBUG: " ~@ms))
    :info `(println "PINTOID:" ~@ms)
    (throw (ex-info "Unknown log level" {:level level}))))


(defmacro goog-base [method & args]
  `(goog/base (~'js* "this") ~(name method) ~@args))


(defmacro goog-extend [type base-type ctor & methods]
  `(do
     (defn ~type ~@ctor)
     (goog/inherits ~type ~base-type)
     ~@(for [[mname & mbody] methods]
         `(set! (.. ~type -prototype ~(symbol (str "-" mname)))
                (fn ~@mbody)))))
