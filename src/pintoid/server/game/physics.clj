(ns pintoid.server.game.physics
  (:use
   [pintoid.server.ecs core system]
   [pintoid.server.data consts])
  (:require
   [taoensso.tufte :as tufte]
   [pintoid.server.vec2 :as v2]
   [taoensso.timbre :as timbre])
  (:import
   [pintoid.server.vec2 Vec2]))

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


;; ===

(defrecord MTree [^double size
                  ^double mass
                  ^Vec2 center
                  ^Vec2 mcenter
                  childs])

(defn- build-mtree-node-or-leaf [w evv]
  (if (<= (count evv) 1)
    (first evv)
    (let [mmc (transduce (map :mcenter) v2/v+ evv)
          m (transduce (map :mass) + evv)
          mc (v2/scale mmc (/ m))
          c (transduce (map :center) v2/v+ evv)
          {mx :x, my :y :as cc} (v2/scale c (/ 1.0 (count evv)))
          sz (transduce (map #(v2/dist-m (:center %) mc)) max 0 evv)
          sz2 (/ sz 3)
          qf (fn [{{x :x, y :y :as c} :center}]
               (let [a (if (< x mx) 1 0)
                     b (if (< y my) 2 0)]
                 (if (< (v2/dist-m c mc) sz2)
                   4 (+ a b))))
          vvv (vals (group-by qf evv))]
      (MTree. (* 2 sz) m mc mmc
              (seq (map #(if (== (count %) (count evv)) %
                             (build-mtree-node-or-leaf w %)) vvv))))))

(defn dedupe-mass-coords [mtt]
  (into []
   (map (fn [ts]
          (if (== 1 (count ts))
            (first ts)
            (let [m (reduce + (map :mass ts))
                  c (:center (first ts))]
              (MTree. 0 m c (v2/scale c m) nil)))))
   (vals (group-by :center mtt))))

(defn- mtree [w]
  (let [evv (into
             []
             (comp
              (map #(let-entity w % [p :position, m :mass]
                      (MTree. 0 m p (v2/scale p m) nil)))
              (filter some?))
             (entities w :phys-act))]
  (build-mtree-node-or-leaf w (dedupe-mass-coords evv))))

(defn mt-gravity [w e mt]
  (let-entity w e [p1 :position, m1 :mass]
    (let [gg gravity-g
          th2 (* gravity-thau gravity-thau)
          g (fn g [^MTree mt]
              (when mt
                (let [ch (.-childs mt)
                      p2 (.-center mt)
                      m2 (.-mass mt)
                      sz (.-size mt)]
                  (let [d2 (v2/dist2 p1 p2)]
                    (if (and ch (> (* sz sz) (* d2 th2)))
                      (transduce (map g) v2/v+' ch)
                      (when-not (zero? d2)
                        (v2/scale (v2/v- p2 p1) (/ (* gg m1 m2) d2))))))))]
      (or (g mt) v2/zero))))

(defn asys-physics-update-vxy [w now]
  (run-timed-system
   w now
   (fn [dt]
     (let [mt (mtree w)]
       (combine-systems!
        (each-entity w eid
            [_  :phys-move
             xy :position
             sfxy [:self-fxy v2/zero]
             ^double m :mass]
          (when (> m 0)
            (let [fxy (v2/v+' (mt-gravity w eid mt) sfxy)
                  axy (v2/scale fxy (/ m))
                  dvxy (limit-vxy (v2/scale axy dt))]
              (fn-> (update-comp! eid :velocity v2/v+' dvxy))))))))))
