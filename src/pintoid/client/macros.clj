(ns pintoid.client.macros)


(defmacro foreach! [[s sq & {xf :xf}] & body]
  `(reduce (~(or xf `identity) (fn [_# ~s] ~@body)) nil ~sq))


(defmacro goog-base [method & args]
  `(goog/base (~'js* "this") ~(name method) ~@args))


(defmacro goog-extend [type base-type ctor & methods]
  `(do
     (defn ~type ~@ctor)
     (goog/inherits ~type ~base-type)
     ~@(for [[mname & mbody] methods]
         `(set! (.. ~type -prototype ~(symbol (str "-" mname)))
                (fn ~@mbody)))))
