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
    (v2/from-polar max-object-velocity (v2/angle vxy))
    vxy))

(defn asys-physics-move [w now]
  (run-timed-system
   w now
   (fn [dt]
     (combine-systems!
      (each-entity w eid
          [xy   :position
           vxy  :velocity]
        (let [dxy (v2/scale vxy dt)]
          (fn-> (update-comp! eid :position v2/v+' dxy))))))))

(defn asys-physics-bound-circle [w now]
  (run-timed-system
   w now
   (fn [dt]
     (combine-systems!
      (each-entity w eid [_ :phys-move
                          xy :position
                          vxy :velocity]
        (let [dd (v2/mag xy)]
          (when (> dd world-jelly-radius)
            (let [dd (v2/mag xy)
                  nxy (v2/norm xy)
                  nvxy (v2/norm vxy)
                  p (+ 1 (v2/dot nxy nvxy))      ;; 0..2 - move in/out of the world
                  x (/ (- dd world-jelly-radius) world-jelly-radius)
                  fv (v2/v+ nxy nxy nvxy)        ;; stoping force direction
                  fs (* -1 p x dt world-jelly-coef) ;; stoping force value
                  dvxy (v2/scale fv fs)
                  f #(v2/v+' % dvxy)]
              (fn-> (update-comp! eid :velocity f)
              )))))))))

(defn asys-physics-update-vxy [w now]
  (run-timed-system
   w now
   (fn [dt]
     (combine-systems!
      (each-entity w eid
          [_    :phys-move
           xy   :position
           m    [:mass 1]
           sfxy [:self-fxy v2/zero]]
        (let [fxy (reduce
                   #(if (= %2 eid)
                      %1
                      (let-entity w %2 [xy2 :position, m2 [:mass 1]]
                        (v2/v+ %1 (calc-gravity-force m m2 xy xy2))))
                   sfxy
                   (entities w :phys-act :position :mass))
              axy (v2/scale fxy (/ m))
              dvxy (limit-vxy (v2/scale axy dt))]
          (fn-> (update-comp! eid :velocity v2/v+' dvxy))))))))
