(ns pintoid.server.game.physics
  (:use
   [pintoid.server utils ecs game-maps])
  (:require
   [pintoid.server.vec2 :as v2]
   [taoensso.timbre :as timbre]
   [pintoid.server.game-maps :as gm]))


(defn sys-physics-move [w dt]
  (into
   w
   (mapcat
    (fn [eid]
      (let [xy (w eid :position)
            fxy (w eid :fxy v2/zero)
            m (w eid :mass 1)
            vxy (w eid :velocity v2/zero)
            axy (v2/scale fxy (/ m))
            vxy' (v2/v+ vxy (v2/scale axy dt))
            dt2 (/ dt 2)
            xy' (v2/v+ xy (v2/scale vxy dt2) (v2/scale vxy' dt2))]
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
                #(v2/v+ %1 (gm/calc-gravity-force m (w %2 :mass) xy (w %2 :position)))
                (w eid :self-fxy v2/zero)
                (eids$ w [:- [:* :phys-act :position :mass] [eid]]))]
        [eid :fxy fxy])))
   (eids$ w [:* :position :mass :phys-move])))


(def sys-simulate-physics
  (make-timed-system
   (fn [w dt]
     (-> w
         (sys-physics-update-vxy dt)
         (sys-physics-move dt)))))
