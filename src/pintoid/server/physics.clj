(ns pintoid.server.physics)

(def gravity-g 50000)
(def force-limit-value 2e-3)
(def engine-forward-force 2e-3)
(def engine-reverse-force 1e-3)

(def bullet-start-velocity 2)
(def bullet-ahead-time 50)
(def bullet-lifetime 4000)
(def bullet-cooldown 300)

(def max-user-view-distance 1500)


(defn hardlimit-force [f]
  (cond
   (> f force-limit-value) force-limit-value
   (< f (- force-limit-value)) (- force-limit-value)
   :t f))


(defn hardlimit-force-2d [[fx fy]]
  [(hardlimit-force fx) (hardlimit-force fy)])


(defn integrate-verle [x0 x1 a dt] 
  (let [x2 (+ (* 2 x1) (- x0) (* a dt dt))]
    x2))


(defn integrate-verle-2d [xy-prev xy a dt] 
  (let [[x0 y0] xy-prev
        [x1 y1] xy
        [ax ay] a]
    [(integrate-verle x0 x1 ax dt)
     (integrate-verle y0 y1 ay dt)]))


(defn dist-decomposition [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2
        dx (- x2 x1)
        dy (- y2 y1)
        r2 (+ (* dx dx) (* dy dy))
        ;; d (.sqrt js/Math r2)
        ]
    [dx dy r2]))


(defn calc-gravity-force [m1 m2 p1 p2]
  (let [g gravity-g
        [rx ry r2] (dist-decomposition p1 p2)
        r (Math/sqrt r2)
        f (* g m1 m2 (/ 1 r2))
        r3' (/ 1 (* r2 r))]
    [(* rx f r3')
     (* ry f r3')]))


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
 (vs* [(Math/cos a) (Math/sin a)] s))
