(ns pintoid.server.math)

(set! *unchecked-math* true)

(defrecord Point [^double x ^double y])
(defrecord Vector [^double x ^double y])

(def ^:const null-vector (Vector. 0 0))

(declare v+)
(declare v-)
(declare vs*)
(declare pv+)
(declare p2v)
(declare sqr)
(declare sqrt)
(declare dist)
(declare dist2)

;; --

(definline sqr [x]
  `(let [x# ~x] (* x# x#)))

(definline sqrt [x]
  `(Math/sqrt ~x))

(defn dist2 [^Point p1 ^Point p2]
  (let [x1 (.-x p1)
        x2 (.-x p2)
        y1 (.-y p1)
        y2 (.-y p2)
        dx (- x2 x1)
        dy (- y2 y1)]
    (+ (sqr dx) (sqr dy))))

(definline dist [p1 p2]
  `(sqrt (dist2 ~p1 ~p2)))

(defn p2v [^Point p1 ^Point p2]
  (Vector.
   (- (.-x p2) (.-x p1))
   (- (.-y p2) (.-y p1))))

(defn pv+
  ([^Point p ^Vector v]
     (Point. (+ (.-x p) (.-x v)) (+ (.-y p) (.-y v))))
  ([^Point p ^Vector v1 ^Vector v2]
     (pv+ p (v+ v1 v2))))

(defn v+
  ([]
     null-vector)
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
     (v- v1 (v+ v2 v3)))
  ([^Vector v1 ^Vector v2 ^Vector v3 & vs]
     (v- v1 (reduce v+ (v+ v2 v3) vs))))

(defn vs*
  ([^Vector v ^double c]
     (Vector. (* c (.-x v)) (* c (.-y v))))
  ([^Vector v ^double c1 ^double c2]
     (vs* v (* c1 c2)))
  ([^Vector v ^double c1 ^double c2 ^double c3]
     (vs* v (reduce * (* c1 c2 c3)))))

(defn vas [^double a ^double s]
  (if (zero? s)
    null-vector
    (Vector.  
     (* (Math/cos a) s)
     (* (Math/sin a) s))))
