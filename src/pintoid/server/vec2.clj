(ns pintoid.server.vec2)

(set! *unchecked-math* true)
(set! *warn-on-reflection* true)

(defrecord Vec2 [^double x ^double y]
  Object
  (toString [_] (str "V" [x y])))

(def ^:const zero (Vec2. 0 0))
(def ^:const one  (Vec2. 1 1))

(definline ^:private sqr [x]
  `(let [x# ~x] (* x# x#)))

(defn vec2
  ([[a b]] (vec2 a b))
  ([a b] (Vec2. a b)))

(defn v+
  ([] zero)
  ([v1] v1)
  ([^Vec2 v1 ^Vec2 v2] (Vec2. (+ (.-x v1) (.-x v2)) (+ (.-y v1) (.-y v2))))
  ([v1 v2 v3] (-> v1 (v+ v2) (v+ v3)))
  ([v1 v2 v3 v4] (-> v1 (v+ v2) (v+ v3) (v+ v4)))
  ([v1 v2 v3 v4 & vs] (reduce v+ (v+ v1 v2 v3 v4) vs)))

(defn v-
  ([^Vec2 v1] (Vec2. (- (.-x v1)) (- (.-y v1))))
  ([^Vec2 v1 ^Vec2 v2] (Vec2. (- (.-x v1) (.-x v2)) (- (.-y v1) (.-y v2))))
  ([v1 v2 v3] (-> v1 (v- v2) (v- v3)))
  ([v1 v2 v3 v4] (-> v1 (v- v2) (v- v3) (v- v4)))
  ([v1 v2 v3 v4 & vs] (reduce v- (v- v1 v2 v3 v4) vs)))

(defn scale [^Vec2 v ^double c]
  (Vec2. (* c (.-x v)) (* c (.-y v))))

(defn dot [^Vec2 v1 ^Vec2 v2]
  (+ (* (.-x v1) (.-x v2))
     (* (.-y v1) (.-y v2))))

(defn mag [^Vec2 v]
  (Math/sqrt (+ (sqr (.-x v))
                (sqr (.-y v)))))

(defn norm [^Vec2 v]
  (let [m (mag v)]
    (Vec2. (/ (.-x v) m)
           (/ (.-y v) m))))

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
  ([[a m]] (from-polar a m))
  ([^double a ^double m]
   (if (zero? m)
     zero
     (Vec2. (* (Math/cos a) m) (* (Math/sin a) m)))))

(defn to-polar [^Vec2 v]
  [(mag v) (angle v)])

(defn to-vec [^Vec2 v]
  (when v [(:x v) (:y v)]))
