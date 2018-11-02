(ns pintoid.server.data.proto
  (:use [pintoid.server.ecs core entity]
        [pintoid.server.data consts assets])
  (:require [pintoid.server.vec2 :as v2]))

(defproto player [& {:keys [position]}]
  {:player true
   :type :player
   :phys-move true
   :fog-of-war true
   :position position
   :fxy v2/zero
   :mass 200
   :angle 0
   :radius 30
   :sprite racket-red
   })

(defproto bullet []
  {:type :bullet
   :sprite bullet-sprite
   :fxy v2/zero
   :phys-move true
   :fog-of-war true
   :mass 100
   :angle 0
   :radius 20
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
   :fog-of-war true
   :mass 8000
   :angle 0
   :radius 10
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
   :phys-act true
   :fog-of-war true
   })

(defproto planet [& {:keys [position mass sprite radius]}]
  {:type :planet
   :position position
   :mass mass
   :sprite sprite
   :radius radius
   :phys-act true
   :fog-of-war true
   })

(defproto asteroid [& {:as cm}]
  (assoc
   cm
   :type :asteroid
   :mass 50
   :radius 5
   :phys-move true
   :fog-of-war true
   ))

(defproto blackhole [& {:as cm}]
  (assoc
   cm
   :type :black
   :mass 10000
   :phys-act true
   :fog-of-war true
   :sprite blackhole-sprite
   :radius 2
   ))

