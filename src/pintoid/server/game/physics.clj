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

(defn sys-physics-move [w dt]
  (entities-reduce! w [:phys-move :position]
    (fn [w' eid]
      (let-entity w eid [xy :position
                         fxy [:fxy v2/zero]
                         m   [:mass 1]
                         vxy [:velocity v2/zero]]
        (let [axy (v2/scale fxy (/ m))
              vxy' (v2/v+ vxy (v2/scale axy dt))
              v (v2/mag vxy')
              vxy' (if (> v max-object-velocity)
                     (v2/from-polar (v2/angle vxy') max-object-velocity)
                     vxy')
              dt2 (/ dt 2)
              xy' (v2/v+ xy (v2/scale vxy dt2) (v2/scale vxy' dt2))]
          (-> w'
              (put-comp! eid :velocity vxy')
              (put-comp! eid :position xy')))))))

(defn sys-physics-update-vxy [w dt]
  (entities-reduce! w :phys-move
    (fn [w' eid]
      (let-entity w eid [xy :position
                         m :mass
                         sfxy [:self-fxy v2/zero]]
        (when (and xy m)
          (let [fxy (reduce
                     #(if (= x eid)
                        %1
                        (let-entity w %2 [xy2 :position, m2 :mass]
                          (v2/v+ %1 (calc-gravity-force m m2 xy xy2))))
                     sfxy
                     (entities w :phys-act :position :mass))]
            (put-comp! w' eid :fxy)))))))

(defn sys-simulate-physics [w now]
  (run-timed-system w now
   (fn [w' dt]
     (-> w'
         (sys-physics-update-vxy dt)
         (sys-physics-move dt)))))
