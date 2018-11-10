(ns pintoid.server.game.collide
  (:use
   [pintoid.server.ecs core system]
   [pintoid.server.data consts])
  (:require
   [pintoid.server.vec2 :as v2]
   [taoensso.timbre :as timbre]))

(defn is-colliding? [w e1 e2]
  ;; TODO: use multimethod here
  (when-not (== e1 e2)
    (let-entity w e1 [r1 :radius, p1 :position]
      (let-entity w e2 [r2 :radius, p2 :position]
        (< (v2/dist p1 p2) (+ r1 r2))))))

(defn asys-collide-entities [w]
  (combine-systems
   ;; TODO: optimize collision detect alg, currently it's O(n^2)!
   (let [;; TODO: use marker component :collidable or :collision-shape
         coll-eids (entities w :radius :position)]
     (for [eid coll-eids]
       (let [cw (vec (filter #(is-colliding? w eid %) coll-eids))]
         (fn [w']
           (put-comp w' eid :collide-with cw)))))))
