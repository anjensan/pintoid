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
   :mass 30
   :angle 0
   :radius 30
   :killable? true
   })


(def bullet-proto
  {:type :bullet
   :texture :ast1
   :fxy [0 0]
   :pxy [0 0]
   :xy [0 0]
   :phys-move true
   :mass 800
   :angle 0
   :radius 30
   :killable? true
   :bullet-lifetime 3000
   :bullet-cooldown 300
   :bullet-velocity 0.5
   ;:phys-act true
   })


(def bullet-alt-proto
  {:type :bullet
   :texture :ast2
   :fxy [0 0]
   :pxy [0 0]
   :xy [0 0]
   :phys-move true
   :phys-act true
   :mass 30
   :angle 0
   :radius 10
   :killable? true
   :bullet-lifetime 3000
   :bullet-cooldown 1000
   :bullet-velocity 1.2
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
    :vxy [1 1]
  })

(defn black-hole [xy dangle] {
    :type :black
    :xy xy
    :mass 500
    :phys-move false
    :phys-act true
    :texture :black1
    :radius 1
    :dangle dangle
  })

(def game-maps
  [[
    (black-hole [-220 -230] 0.2)
    (star [-2100 -2100] 50 33 :star1 0.1)
    (star [1200 500] 200 20 :star2 0.1)
    (star [-900 -700] 100 66 :star3 0.1)
    (star [400 300] 175 70 :star4 0.1)
    (star [-2400 -655] 200 25 :star2 0.1)
    (star [1100 -1700] 100 66 :star3 0.1)
    (star [-1600 1000] 175 70 :star4 0.1)
    (planet [-2440 -900] 15 10 :green_planet1 0.1)
    (planet [100 -100] 10 9 :green_planet1 0.1)
    (planet [900 -2440] 20 11 :pink_planet1 0.1)
    (planet [100 2100] 15 8 :pink_planet1 0.1)
    (planet [-1100 1100] 12 10 :pink_planet1 0.1)
    (planet [-440 -200] 15 10 :green_planet1 0.1)
    (planet [2300 -100] 10 9 :green_planet1 0.1)
    (planet [-900 -2240] 20 11 :pink_planet1 0.1)
    (planet [1020 -2100] 15 8 :pink_planet1 0.1)
    (planet [-100 1100] 12 10 :pink_planet1 0.1)
    (asteroid [-220 -232] 5 3 :ast1 0.1)
    (asteroid [-222 -233] 5 3 :ast2 0.1)
    (asteroid [-221 -234] 4 3 :ast3 0.1)
    (asteroid [-224 -236] 7 3 :ast4 0.1)
    (asteroid [-2100 -2130] 8 3 :ast5 0.1)
    (asteroid [1200 510] 9 3 :ast2 0.1)
    (asteroid [-900 -700] 7 3 :ast3 0.1)
    (asteroid [1100 -1700] 8 3 :ast1 0.1)
    (asteroid [1100 -1710] 8 3 :ast1 0.1)
   ]])
