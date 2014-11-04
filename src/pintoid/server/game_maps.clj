(ns pintoid.server.game-maps
  (:use pintoid.server.math))

(def world-height 2500)
(def world-width 2500)

(def gravity-g 0.005)
(def engine-forward-force 0.12)
(def engine-reverse-force 0.05)

(def bullet-ahead-time 200)
(def max-user-view-distance 1500)

(defn calc-gravity-force [^double m1 ^double m2 p1 p2]
  (let [r' (/ (dist p1 p2))
        f (* gravity-g m1 r' m2 r')]
    (vs* (p2v p1 p2) (* f r'))))


(def player-proto
  {:player true
   :type :player
   :fxy (->Vector 0 0)
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
   :fxy (->Vector 0 0)
   :xy (->Point 0 0)
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
   :fxy (->Vector 0 0)
   :xy (->Point 0 0)
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
    (black-hole (->Point 0 0) 0.2)
    (star (->Point -2100 -1350) 500 33 :star1 0.1)
    (star (->Point 1200 500) 2000 20 :star2 0.1)
    (star (->Point -900 -700) 1000 66 :star3 0.1)
    (star (->Point 400 300) 175 70 :star4 0.1)
    (star (->Point -2400 -655) 2000 25 :star2 0.1)
    (star (->Point 1100 -1700) 1000 66 :star3 0.1)
    (star (->Point -1600 1000) 1750 70 :star4 0.1)
    (planet (->Point -2440 -900) 150 10 :green_planet1 0.1)
    (planet (->Point 100 -100) 100 9 :green_planet1 0.1)
    (planet (->Point 900 -2440) 200 11 :pink_planet1 0.1)
    (planet (->Point 100 2100) 150 8 :pink_planet1 0.1)
    (planet (->Point -1100 1100) 120 10 :pink_planet1 0.1)
    (planet (->Point -440 -200) 150 10 :green_planet1 0.1)
    (planet (->Point 2300 -100) 100 9 :green_planet1 0.1)
    (planet (->Point -900 -2240) 200 11 :pink_planet1 0.1)
    (planet (->Point 1020 -2100) 150 8 :pink_planet1 0.1)
    (planet (->Point -100 1100) 120 10 :pink_planet1 0.1)
    (asteroid (->Point -1220 -1232) 50 3 :ast1 0.1)
    (asteroid (->Point -200 -300) 50 3 :ast2 0.1)
    (asteroid (->Point -400 -200) 40 3 :ast3 0.1)
    (asteroid (->Point -200 -1200) 70 3 :ast4 0.1)
    (asteroid (->Point -2100 -2100) 80 3 :ast5 0.1)
    (asteroid (->Point 1200 510) 90 3 :ast2 0.1)
    (asteroid (->Point -920 -700) 70 3 :ast3 0.1)
    (asteroid (->Point 1000 -1500) 80 3 :ast1 0.1)
    (asteroid (->Point 1100 -1810) 80 3 :ast1 0.1)
   ]])
