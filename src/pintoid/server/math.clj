(ns pintoid.server.math)

(set! *unchecked-math* true)

(defrecord Vector [^double x ^double y])
(def ^:const vector-0 (Vector. 0 0))

(declare abs)
(declare sqr)
(declare sqrt)

(declare v+)
(declare v-)
(declare vs*)
(declare vn)
(declare vl)
(declare vv*)

(declare dist)
(declare dist-m)

;; --

(definline sqr [x]
  `(let [x# ~x] (* x# x#)))

(definline abs [x]
  `(let [x# ~x] (if (< 0 x#) (- x#) x#)))

(definline sqrt [x]
  `(Math/sqrt ~x))

(defn v+
  ([]
     #pintoid.server.math.Vector[0 0])
  ([^Vector v]
     v)
  ([^Vector v1 ^Vector v2]
     (Vector. (+ (.-x v1) (.-x v2)) (+ (.-y v1) (.-y v2))))
  ([^Vector v1 ^Vector v2 ^Vector v3]
     (v+ (v+ v1 v2) v3))
  ([^Vector v1 ^Vector v2 ^Vector v3 & vs]
     (reduce v+ (v+ v1 v2 v3) vs)))

(defn v-
  ([^Vector v] 
     (Vector. (- (.-x v)) (- (.-y v))))
  ([^Vector v1 ^Vector v2]
     (Vector. (- (.-x v1) (.-x v2)) (- (.-y v1) (.-y v2))))
  ([^Vector v1 ^Vector v2 ^Vector v3]
     (v- (v- v1 v2) v3))
  ([^Vector v1 ^Vector v2 ^Vector v3 & vs]
     (reduce v- (v- v1 v2 v3) vs)))

(defn vv*
  ([^Vector v1 ^Vector v2]
     (+ (* (.-x v1) (.-x v2)) (* (.-y v1) (.-y v2)))))

(defn vl
  ([^Vector v]
     (sqrt (+ (sqr (.-x v)) (sqr (.-y v))))))

(defn vn
  ([^Vector v]
     (let [d (/ (vl v))]
       (Vector. (* (.-x v) d) (* (.-y v) d)))))

(defn vs*
  ([]
     #pintoid.server.math.Vector[1 1])
  ([^Vector v ^double c]
     (Vector. (* c (.-x v)) (* c (.-y v))))
  ([^Vector v ^double c1 ^double c2]
     (vs* v (* c1 c2)))
  ([^Vector v ^double c1 ^double c2 ^double c3]
     (vs* v (reduce * (* c1 c2 c3)))))

(defn vas [^double a ^double s]
  (if (zero? s)
    #pintoid.server.math.Vector[0 0]
    (Vector.  
     (* (Math/cos a) s)
     (* (Math/sin a) s))))

(definline dist [v1 v2]
  `(vl (v- ~v2 ~v1)))

(definline dist-m [v1 v2]
  `(let [^Vector v1# ~v1 ^Vector v2# ~v2]
     (+ (abs (- (.-x v1#) (.-x v2#)))
        (abs (- (.-y v1#) (.-y v2#))))))
