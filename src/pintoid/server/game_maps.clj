(ns pintoid.server.game-maps)

(def default-entity
  {:type :clojure
   :xy [100 100]
   :fxy [0 0]
   :phys-move false
   :phys-act false
   :mass 0
   :texture :clojure
   :dangle 0
   :angle 0
   })


(def player-proto
  {:type :player
   :fxy [0 0]
   :phys-move true
   :mass 10
   :angle 0
   :radius 20
   })


(def bullet-proto
  {:type :rocket
   :fxy [0 0]
   :phys-move true
   :mass 1
   :angle 0
   :radius 2
   })


(def game-maps
  [[
   {:type :star
    :xy [300 400]
    :mass 100
    :phys-move false ; false
    :phys-act true
    :texture :star1
    :radius 33
    :dangle 0.1
    }
   ]])
