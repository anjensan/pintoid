(ns pintoid.server.physics)

(def world-height 2500)
(def world-width 2500)

(def gravity-g 0.005)
(def engine-forward-force 0.12)
(def engine-reverse-force 0.05)

(def bullet-ahead-time 200)
(def max-user-view-distance 1500)


(defn dist-decomposition [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2
        dx (- x2 x1)
        dy (- y2 y1)
        r2 (+ (* dx dx) (* dy dy))
        r (Math/sqrt r2)]
    [(/ dx r) (/ dy r) r]))

(defn calc-gravity-force [m1 m2 p1 p2]
  (let [[rx ry r] (dist-decomposition p1 p2)
        r' (/ r)
        f (* gravity-g m1 m2 r' r')]
    [(* rx f)
     (* ry f)]))

(definline sqr [x]
  `(let [x# ~x] (* x# x#)))

(defn distance2 [[x1 y1] [x2 y2]] 
  (let [dx (- x1 x2)
        dy (- y1 y2)]
    (+ (* dx dx) (* dy dy))))

(defn distance [xy1 xy2] 
  (Math/sqrt (distance2 xy1 xy2)))

(defn v+ [p1 p2] 
  (let [[x1 y1] p1
        [x2 y2] p2]
    [(+ x1 x2)
     (+ y1 y2)]))

(defn v- [p1 p2] 
  (let [[x1 y1] p1
        [x2 y2] p2]
    [(- x1 x2)
     (- y1 y2)]))

(defn vs* [p c]
  (let [[x1 y1] p]
    [(* x1 c)
     (* y1 c)]))

(defn vas [a s]
  [(* (Math/cos a) s) (* (Math/sin a) s)])
