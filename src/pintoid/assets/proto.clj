(ns pintoid.assets.proto
  (:use [pintoid.ecs.entity]
        [pintoid.assets.sprites])
  (:require [pintoid.server.vec2 :as v2]))

(defproto player [& {:keys [position score nick] :or {score 0}}]
  {:type :player
   :player {:score score :nick nick}
   :phys-move true
   :fog-of-war true
   :position position
   :fxy v2/zero
   :mass 100
   :angle 0
   :collide :circle
   :radius 20
   :sprite racket-red
   })

(defproto bullet []
  {:type :bullet
   :sprite bullet-sprite
   :fxy v2/zero
   :phys-move true
   :fog-of-war true
   :mass 3
   :angle 0
   :radius 17
   :collide :circle
   :bullet {:lifetime 8000
            :cooldown 100
            :velocity 0.75}
   })

(defproto bullet-alt []
  {:type :bullet
   :sprite bullet-alt-sprite
   :fxy (v2/vec2 0 0)
   :position (v2/vec2 0 0)
   :phys-move true
   :phys-act true
   :fog-of-war true
   :mass 100
   :angle 0
   :radius 20
   :collide :circle
   :bullet {:lifetime 15000
            :cooldown 800
            :velocity 2.5}
   })

(defproto star [& {:keys [position mass sprite radius]}]
  {:type :star
   :position position
   :mass mass
   :sprite sprite
   :radius radius
   :collide :circle
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
   :collide :circle
   :phys-act true
   :fog-of-war true
   })

(defproto asteroid [& {:as cm}]
  (assoc
   cm
   :type :asteroid
   :mass 20
   :radius 10
   :collide :circle
   :phys-move true
   :fog-of-war true
   ))

(defproto blackhole [& {:as cm}]
  (assoc
   cm
   :type :black
   :phys-act true
   :fog-of-war true
   :radius 1
   :collide :circle
   :sprite blackhole-sprite
   :radius 2
   ))

