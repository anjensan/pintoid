(ns pintoid.server.vec2
  (:use clojure.template))

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(defrecord Vec2 [^double x ^double y]
  Object
  (toString [_] (str [x y])))

(def ^:const zero (Vec2. 0 0))
(def ^:const one  (Vec2. 1 1))

(definline ^:private sqr [x]
  `(let [x# ~x] (* x# x#)))

(defn vec2 [a b]
  (Vec2. (double a) (double b)))

(defn v+
  ([] zero)
  ([^Vec2 v1] v1)
  ([^Vec2 v1 ^Vec2 v2] (Vec2. (+ (.-x v1) (.-x v2)) (+ (.-y v1) (.-y v2))))
  ([^Vec2 v1 ^Vec2 v2 ^Vec2 v3] (v+ (v+ v1 v2) v3))
  ([^Vec2 v1 ^Vec2 v2 ^Vec2 v3 & vs] (reduce v+ (v+ v1 v2 v3) vs)))

(defn v-
  ([^Vec2 v1] (Vec2. (- (.-x v1)) (- (.-y v1))))
  ([^Vec2 v1 ^Vec2 v2] (Vec2. (- (.-x v1) (.-x v2)) (- (.-y v1) (.-y v2))))
  ([^Vec2 v1 ^Vec2 v2 ^Vec2 v3] (v- (v- v1 v2) v3))
  ([^Vec2 v1 ^Vec2 v2 ^Vec2 v3 & vs] (reduce v- (v- v1 v2 v3) vs)))

(defn dot [^Vec2 v1 ^Vec2 v2]
  (+ (* (.-x v1) (.-x v2))
     (* (.-y v1) (.-y v2))))

(defn mag [^Vec2 v]
  (Math/sqrt
   (+ (Math/pow (.-x v) 2)
      (Math/pow (.-y v) 2))))

(defn norm [^Vec2 v]
  (let [m (mag v)]
    (Vec2. (/ (.-x v) m)
              (/ (.-y v) m))))

(defn scale
  ([] one)
  ([^Vec2 v ^double c] (Vec2. (* c (.-x v)) (* c (.-y v))))
  ([^Vec2 v ^double c1 ^double c2] (scale v (* c1 c2)))
  ([^Vec2 v ^double c1 ^double c2 ^double c3] (scale v (reduce * (* c1 c2 c3)))))

(defn dist [^Vec2 v1 ^Vec2 v2]
  (mag (v- v2 v1)))

(defn dist-m [^Vec2 v1 ^Vec2 v2]
  (+ (Math/abs (- (.-x v1) (.-x v2)))
     (Math/abs (- (.-y v1) (.-y v2)))))

(defn angle [^Vec2 v]
  (Math/atan2 (.-x v) (.-y v)))

(defn rotate [^Vec2 v ^double a]
  (let [sa (Math/sin a)
        ca (Math/cos a)
        x (.-x v)
        y (.-y v)
        nx (- (* x ca) (* y sa))
        ny (+ (* x sa) (* y ca))]
    (Vec2. nx ny)))

(defn from-polar
  ([[a s]] (from-polar a s))
  ([^double a ^double s]
   (if (zero? s)
     zero
     (Vec2. (* (Math/cos a) s) (* (Math/sin a) s)))))

(defn to-polar [^Vec2 v]
  [(mag v) (angle v)])
