(ns pintoid.server.game.physics
  (:use
   [pintoid.server.ecs core system]
   [pintoid.server.data consts])
  (:require
   [pintoid.server.vec2 :as v2]
   [taoensso.timbre :as timbre]))

(defn calc-gravity-force [^double m1 ^double m2 p1 p2]
  (let [d (v2/dist p1 p2)]
    (if (zero? d)
      v2/zero  ; FIXME: remove this?
      (let [r' (/ d)
            fx (* gravity-g r' m1 r' m2 r')]
        (v2/scale (v2/v- p2 p1) fx)))))

(defn- limit-vxy [vxy]
  (if (> (v2/mag vxy) max-object-velocity)
    (v2/from-polar (v2/angle vxy) max-object-velocity)
    vxy))

(defn asys-physics-move [w now]
  (run-timed-system
   w now
   (fn [dt]
     (comp-system!
      (each-entity w eid
          [xy   :position
           vxy  :velocity]
        (let [xy' (v2/v+ xy (v2/scale vxy dt))]
          (fn-> (put-comp! eid :position xy'))))))))

(defn asys-physics-update-vxy [w now]
  (run-timed-system
   w now
   (fn [dt]
     (comp-system!
      (each-entity w eid
          [_    :phys-move
           xy   :position
           m    [:mass 1]
           vxy  [:velocity v2/zero]
           sfxy [:self-fxy v2/zero]]
        (let [fxy (reduce
                   #(if (= %2 eid)
                      %1
                      (let-entity w %2 [xy2 :position, m2 :mass]
                        (v2/v+ %1 (calc-gravity-force m m2 xy xy2))))
                   sfxy
                   (entities w :phys-act :position :mass))
              axy (v2/scale fxy (/ m))
              vxy' (limit-vxy (v2/v+ vxy (v2/scale axy dt)))]
          (fn-> (put-comp! eid :velocity vxy'))))))))

(defn asys-simulate-physics [w now]
  (let [a (future (asys-physics-update-vxy w now))
        b (future (asys-physics-move w now))]
    (fn-> (@a) (@b))))
