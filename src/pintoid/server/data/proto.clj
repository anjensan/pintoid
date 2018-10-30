(ns pintoid.server.data.proto
  (:use [pintoid.server.ecs core entity]
        [pintoid.server.data consts assets])
  (:require [pintoid.server.vec2 :as v2]))

(defn- visible-by-player? [w pid eid]
  (< (v2/dist (w eid :position) (w pid :position)) max-user-view-distance))

(defproto player [& {:keys [position]}]
  {:player true
   :type :player
   :position position
   :fxy v2/zero
   :phys-move true
   :mass 200
   :angle 0
   :radius 30
   :sprite racket-red
   :visible? (constantly true)
   })

(defproto bullet []
  {:type :bullet
   :sprite bullet-sprite
   :fxy v2/zero
   :phys-move true
   :mass 100
   :angle 0
   :radius 20
   :visible? visible-by-player?
   :bullet {:lifetime 5000
            :cooldown 200
            :velocity 1.0}
   })

(defproto bullet-alt []
  {:type :bullet
   :sprite bullet-sprite
   :fxy (v2/vec2 0 0)
   :position (v2/vec2 0 0)
   :phys-move true
   :phys-act true
   :mass 8000
   :angle 0
   :radius 10
   :visible? visible-by-player?
   :bullet {:lifetime 10000
            :cooldown 1200
            :velocity 2.1}
   })

(defproto star [& {:keys [position mass sprite radius]}]
  {:type :star
   :position position
   :mass mass
   :sprite sprite
   :radius radius
   :layer lstars1
   :visible? visible-by-player?
   :phys-act true
   })

(defproto planet [& {:keys [position mass sprite radius]}]
  {:type :planet
   :position position
   :mass mass
   :sprite sprite
   :radius radius
   :visible? visible-by-player?
   :phys-act true
   })

(defproto asteroid [& {:as cm}]
  (assoc
   cm
   :type :asteroid
   :mass 50
   :radius 5
   :visible? visible-by-player?
   :phys-move true
   ))

(defproto blackhole [& {:as cm}]
  (assoc
   cm
   :type :black
   :mass 10000
   :phys-act true
   :sprite blackhole-sprite
   :radius 2
   :visible? visible-by-player?
   ))

