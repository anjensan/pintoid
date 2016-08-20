(ns pintoid.client.macros)


(defmacro foreach! [[s sq & {xf :xf}] & body]
  `(do
     (reduce (~(or xf `identity) (fn [_# ~s] ~@body)) nil ~sq)
     nil))


(defn- ctor-with-this-arg [[[this & args] & body]]
  `(fn [~@args]
     (cljs.core/this-as this#
      (let [~this this#]
        ~@body
        this#))))


(defn- fn-with-this-arg [[[this & args] & body]]
  `(fn [~@args] (cljs.core/this-as ~this ~@body)))


(defmacro call-super
  [class this method & args]
  (if (string? method)
    `(.apply
      (aget (.getPrototypeOf js/Object (.-prototype ~class)) ~method)
      ~this
      (cljs.core/array ~@args))
    (let [m (name method)
          n (if (re-matches #"\..+" m) (subs m 1) m)
          mi (symbol (str ".-" n))]
      `(.apply
        (~mi (.getPrototypeOf js/Object (.-prototype ~class)))
        ~this
        (cljs.core/array ~@args)))))


(defmacro defjsclass [class-name parent-class & ctor-and-methods]
  (let [ctor-body? (fn [[x & _]] (or (vector? x) (= 'constructor x)))
        [[ctor] methods] (split-with ctor-body? ctor-and-methods)
        ctor-body (if (-> ctor first vector?) ctor (rest ctor))]
    (assert ctor "No constructor")
    `(do
       (def
         ~(vary-meta class-name assoc :jsdoc ["@constructor"])
         ~(ctor-with-this-arg ctor-body))
       (goog/inherits ~class-name ~parent-class)
       ~@(for [[mname & mbody] methods]
           (if (string? mname)
             `(aset (.. ~class-name -prototype) ~(str mname) ~(fn-with-this-arg mbody))
             `(set! (.. ~class-name -prototype ~(symbol (str "-" mname))) ~(fn-with-this-arg mbody))
             )))))
