(ns pintoid.server.game.collide
  (:use
   [pintoid.server utils ecs game-maps])
  (:require
   [pintoid.server.vec2 :as v2]
   [taoensso.timbre :as timbre]
   [pintoid.server.game-maps :as gm]))


(defn is-colliding? [w e1 e2]
  ;; TODO: use multimethod here
  (when-not (== e1 e2)
    (when-some* [r1 (w e1 :radius)
                 r2 (w e2 :radius)
                 p1 (w e1 :position)
                 p2 (w e2 :position)]
      (< (v2/dist p1 p2) (+ r1 r2)))))


(defn sys-collide-entities [w]
  ;; TODO: optimize collision detect alg, currently it's O(n^2)!
  (let [;; TODO: use marker component :collidable or :collision-shape
        coll-eids (entities w :radius :position)]
    (reduce
     (fn [w eid]
       (put-comp
        w eid :collide-with
        (seq (filter #(is-colliding? w eid %) coll-eids))))
     w
     coll-eids)))
