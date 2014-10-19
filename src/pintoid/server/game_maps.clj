(ns pintoid.server.game-maps)

(def default-entity
  {:type :clojure
   :xy [0 0]
   :pxy [0 0]
   :fxy [0 0]
   :vxy [0 0]
   :phys-move false
   :phys-act false
   :mass 0
   :texture :clojure
   :dangle 0
   :angle 0
   :killable? false
   })


(def player-proto
  {:type :player
   :fxy [0 0]
   :phys-move true
   :mass 10
   :angle 0
   :radius 20
   :killable? true
   })


(def bullet-proto
  {:type :bullet
   :texture :ast1
   :fxy [0 0]
   :pxy [0 0]
   :xy [0 0]
   :phys-move true
   :mass 1
   :angle 0
   :radius 2
   :killable? true
   })

(defn star [xy mass radius texture dangle] {
    :type :star
    :xy xy
    :mass mass
    :phys-move false 
    :phys-act true
    :texture texture
    :radius radius
    :dangle dangle
  })

(defn planet [xy mass radius texture dangle] {
    :type :planet
    :xy xy
    :mass mass
    :phys-move false 
    :phys-act true
    :texture texture
    :radius radius
    :dangle dangle
  })

(defn asteroid [xy mass radius texture dangle] {
    :type :ast
    :xy xy
    :mass mass
    :phys-move true 
    :phys-act false
    :texture texture
    :radius radius
    :dangle dangle
  })

(defn black-hole [xy dangle] {
    :type :black
    :xy xy
    :mass 500
    :phys-move false 
    :phys-act true
    :texture :black
    :radius 1
    :dangle dangle
  })

(def game-maps
  [[
    (star [600 800] 100 33 :star1 0.1)
    (planet [400 600] 50 30 :green_planet1 0.1)
    (asteroid [100 159] 25 5 :ast1 0.1)
    (black-hole [250 250] 0.2)
   ]])
