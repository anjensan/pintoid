(ns pintoid.server.vec2)

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

(defrecord Vec2 [^double x ^double y]
  Object
  (toString [_] (str "V" [x y])))

(def ^:const zero (Vec2. 0 0))
(def ^:const one  (Vec2. 1 1))

(defmacro ^:private sqr [x]
  `(let [x# ~x] (* x# x#)))

(defn vec2
  ([[a b]] (vec2 a b))
  ([a b] (Vec2. a b)))

(defn vnan? [^Vec2 v]
  (or (Double/isNaN (.-x v))
      (Double/isNaN (.-y v))))

(defn vzero?
  ([^Vec2 v]
   (and
    (== 0 (.-x v))
    (== 0 (.-y v))))
  ([^Vec2 v ^double eps]
   (and
    (<= (Math/abs (.-x v)) eps)
    (<= (Math/abs (.-y v)) eps))))

(defmacro not-nan [v]
  `(do (assert (not (vnan? ~v))) ~v))

(defn v+
  ([] zero)
  ([v1] v1)
  ([^Vec2 v1 ^Vec2 v2] (Vec2. (+ (.-x v1) (.-x v2)) (+ (.-y v1) (.-y v2))))
  ([v1 v2 v3] (-> v1 (v+ v2) (v+ v3)))
  ([v1 v2 v3 v4] (-> v1 (v+ v2) (v+ v3) (v+ v4)))
  ([v1 v2 v3 v4 & vs] (reduce v+ (v+ v1 v2 v3 v4) vs)))

(defn v+'
  ([] zero)
  ([v1] (or v1 zero))
  ([^Vec2 v1 ^Vec2 v2]
   (if (and (some? v1) (some? v2))
     (v+ v1 v2)
     (or v1 v2 zero)))
  ([v1 v2 v3] (-> v1 (v+' v2) (v+' v3)))
  ([v1 v2 v3 v4] (-> v1 (v+' v2) (v+' v3) (v+' v4)))
  ([v1 v2 v3 v4 & vs] (reduce v+' (v+' v1 v2 v3 v4) vs)))

(defn v-
  ([^Vec2 v1] (Vec2. (- (.-x v1)) (- (.-y v1))))
  ([^Vec2 v1 ^Vec2 v2] (Vec2. (- (.-x v1) (.-x v2)) (- (.-y v1) (.-y v2))))
  ([v1 v2 v3] (-> v1 (v- v2) (v- v3)))
  ([v1 v2 v3 v4] (-> v1 (v- v2) (v- v3) (v- v4)))
  ([v1 v2 v3 v4 & vs] (reduce v- (v- v1 v2 v3 v4) vs)))

(defn scale [^Vec2 v ^double c]
  (Vec2. (* c (.-x v)) (* c (.-y v))))

(defn dot ^double [^Vec2 v1 ^Vec2 v2]
  (+ (* (.-x v1) (.-x v2))
     (* (.-y v1) (.-y v2))))

(defn mag ^double [^Vec2 v]
  (Math/sqrt (+ (sqr (.-x v))
                (sqr (.-y v)))))

(defn norm [^Vec2 v]
  (let [m (mag v)]
    (if (== 0 m)
      zero
      (Vec2. (/ (.-x v) m)
             (/ (.-y v) m)))))

(defn dist ^double [^Vec2 v1 ^Vec2 v2]
  (mag (v- v2 v1)))

(defn dist2 ^double [^Vec2 v1 ^Vec2 v2]
  (+ (sqr (- (.-x v1) (.-x v2)))
     (sqr (- (.-y v1) (.-y v2)))))

(defn distm ^double [^Vec2 v1 ^Vec2 v2]
  (+ (Math/abs (- (.-x v1) (.-x v2)))
     (Math/abs (- (.-y v1) (.-y v2)))))

(defn angle ^double [^Vec2 v]
  (Math/atan2 (.-y v) (.-x v)))

(defn rotate [^Vec2 v ^double a]
  (let [sa (Math/sin a)
        ca (Math/cos a)
        x (.-x v)
        y (.-y v)
        nx (- (* x ca) (* y sa))
        ny (+ (* x sa) (* y ca))]
    (Vec2. nx ny)))

(defn from-polar
  ([[m a]]
   (from-polar a m))
  ([^double m ^double a]
   (if (zero? m)
     zero
     (Vec2. (* (Math/cos a) m)
            (* (Math/sin a) m)))))

(defn to-polar [^Vec2 v]
  [(mag v) (angle v)])

(defn to-vec [^Vec2 v]
  (when v [(.-x v) (.-y v)]))
