(ns pintoid.server.game-maps)


(def player-proto
  {:player true
   :type :player
   :fxy [0 0]
   :phys-move true
   :mass 300
   :angle 0
   :radius 30
   :killable? true
   })


(def bullet-proto
  {:bullet true
   :type :bullet
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
   })


(def bullet-alt-proto
  {:bullet true
   :type :bullet
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

(defn star [xy mass radius texture dangle]
  {
   :type :star
   :xy xy
   :mass mass
   :phys-act true
   :texture texture
   :radius radius
   :dangle dangle
   })

(defn planet [xy mass radius texture dangle]
  {
   :type :planet
   :xy xy
   :mass mass
   :phys-act true
   :texture texture
   :radius radius
   :dangle dangle
   })

(defn asteroid [xy mass radius texture dangle]
  {
   :type :ast
   :xy xy
   :mass mass
   :phys-move true
   :texture texture
   :radius radius
   :dangle dangle
   :vxy [1 1]
   })

(defn black-hole [xy dangle]
  {
   :type :black
   :xy xy
   :mass 50000
   :phys-act true
   :texture :black1
   :radius 1
   :dangle dangle
   })

(def game-maps
  [[
    (black-hole [0 0] 0.2)
    (star [-2100 -2100] 500 33 :star1 0.1)
    (star [1200 500] 2000 20 :star2 0.1)
    (star [-900 -700] 1000 66 :star3 0.1)
    (star [400 300] 175 700 :star4 0.1)
    (star [-2400 -655] 2000 25 :star2 0.1)
    (star [1100 -1700] 1000 66 :star3 0.1)
    (star [-1600 1000] 1750 70 :star4 0.1)
    (planet [-2440 -900] 150 10 :green_planet1 0.1)
    (planet [100 -100] 100 9 :green_planet1 0.1)
    (planet [900 -2440] 200 11 :pink_planet1 0.1)
    (planet [100 2100] 150 8 :pink_planet1 0.1)
    (planet [-1100 1100] 120 10 :pink_planet1 0.1)
    (planet [-440 -200] 150 10 :green_planet1 0.1)
    (planet [2300 -100] 100 9 :green_planet1 0.1)
    (planet [-900 -2240] 200 11 :pink_planet1 0.1)
    (planet [1020 -2100] 150 8 :pink_planet1 0.1)
    (planet [-100 1100] 120 10 :pink_planet1 0.1)
    (asteroid [-1220 -1232] 50 3 :ast1 0.1)
    (asteroid [-200 -300] 50 3 :ast2 0.1)
    (asteroid [-400 -200] 40 3 :ast3 0.1)
    (asteroid [-200 -1200] 70 3 :ast4 0.1)
    (asteroid [-2100 -2100] 80 3 :ast5 0.1)
    (asteroid [1200 510] 90 3 :ast2 0.1)
    (asteroid [-920 -700] 70 3 :ast3 0.1)
    (asteroid [1000 -1500] 80 3 :ast1 0.1)
    (asteroid [1100 -1810] 80 3 :ast1 0.1)
   ]])
