(ns pintoid.server.game.physics
  (:use
   [pintoid.server utils math ecs game-maps])
  (:require
   [taoensso.timbre :as timbre]
   [pintoid.server.game-maps :as gm]))


(defn sys-physics-move [w dt]
  (into
   w
   (mapcat
    (fn [eid]
      (let [xy (w eid :position)
            fxy (w eid :fxy vector-0)
            m (w eid :mass 1)
            vxy (w eid :velocity vector-0)
            axy (vs* fxy (/ m))
            vxy' (v+ vxy (vs* axy dt))
            dt2 (/ dt 2)
            xy' (v+ xy (vs* vxy dt2) (vs* vxy' dt2))]
        [[eid :velocity vxy']
         [eid :position xy']])))
    (eids$ w [:* :position :phys-move [:+ :velocity :fxy]])))


(defn sys-physics-update-vxy [w dt]
  (into
   w
   (map
    (fn [eid]
      (let [xy (w eid :position)
            m (w eid :mass 1)
           fxy (reduce
                #(v+ %1 (gm/calc-gravity-force m (w %2 :mass) xy (w %2 :position)))
                (w eid :self-fxy vector-0)
                (eids$ w [:- [:* :phys-act :position :mass] [eid]]))]
        [eid :fxy fxy])))
   (eids$ w [:* :position :mass :phys-move])))


(def sys-simulate-physics
  (make-timed-system
   (fn [w dt]
     (-> w
         (sys-physics-update-vxy dt)
         (sys-physics-move dt)))))
