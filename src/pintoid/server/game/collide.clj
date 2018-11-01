(ns pintoid.server.game.collide
  (:use
   [pintoid.server.ecs core]
   [pintoid.server.data consts])
  (:require
   [pintoid.server.vec2 :as v2]
   [taoensso.timbre :as timbre]))

(defn is-colliding? [w e1 e2]
  ;; TODO: use multimethod here
  (when-not (== e1 e2)
    (let-entity w e1 [r1 :radius, p1 :position]
      (when (and r1 p1)
        (let-entity w e2 [r2 :radius, p2 :position]
          (when (and r2 p2)
            (< (v2/dist p1 p2) (+ r1 r2))))))))

(defn sys-collide-entities [w]
  ;; TODO: optimize collision detect alg, currently it's O(n^2)!
  (let [;; TODO: use marker component :collidable or :collision-shape
        coll-eids (entities w [:radius :position])]
    (reduce
     (fn [w eid]
       (put-comp
        w eid :collide-with
        (seq (filter #(is-colliding? w eid %) coll-eids))))
     w
     coll-eids)))
