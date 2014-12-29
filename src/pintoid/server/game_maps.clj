(ns pintoid.server.game-maps
  (:use pintoid.server.math))

(def world-height 2500)
(def world-width 2500)

(def gravity-g 0.005)
(def engine-forward-force 0.12)
(def engine-reverse-force 0.05)
(def rotate-speed 0.15)

(def bullet-ahead-time 200)
(def max-user-view-distance 1500)

(defn calc-gravity-force [^double m1 ^double m2 p1 p2]
  (let [d (dist p1 p2)]
    (if (zero? d)
      (->Vector 0 0)  ; FIXME: remove this?
      (let [r' (/ d)
            fx (* gravity-g r' m1 r' m2 r')]
        (vs* (v- p2 p1) fx)))))


(def player-proto
  {:player true
   :type :player
   :fxy (->Vector 0 0)
   :phys-move true
   :mass 100
   :angle 0
   :radius 30
   })


(def bullet-proto
  {:type :bullet
   :texture :ast1
   :fxy (->Vector 0 0)
   :xy (->Vector 0 0)
   :phys-move true
   :mass 50
   :angle 0
   :radius 20
   :bullet {:lifetime 5000
            :cooldown 200
            :velocity 1.0}
   })

(def bullet-alt-proto
  {:type :bullet
   :texture :ast2
   :fxy (->Vector 0 0)
   :xy (->Vector 0 0)
   :phys-move true
   :phys-act true
   :mass 2000
   :angle 0
   :radius 10
   :bullet {:lifetime 10000
            :cooldown 1200
            :velocity 2.1}
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
   :mass 5000
   :phys-act true
   :texture :black1
   :radius 1
   :dangle dangle
   })

(def game-maps
  [[
    (black-hole (->Vector 0 0) 0.2)
    (star (->Vector -2100 -1350) 500 33 :star1 0.1)
    (star (->Vector 1200 500) 2000 20 :star2 0.1)
    (star (->Vector -900 -700) 1000 66 :star3 0.1)
    (star (->Vector 400 300) 175 70 :star4 0.1)
    (star (->Vector -2400 -655) 2000 25 :star2 0.1)
    (star (->Vector 1100 -1700) 1000 66 :star3 0.1)
    (star (->Vector -1600 1000) 1750 70 :star4 0.1)
    (planet (->Vector -2440 -900) 150 10 :green_planet1 0.1)
    (planet (->Vector 100 -100) 100 9 :green_planet1 0.1)
    (planet (->Vector 900 -2440) 200 11 :pink_planet1 0.1)
    (planet (->Vector 100 2100) 150 8 :pink_planet1 0.1)
    (planet (->Vector -1100 1100) 120 10 :pink_planet1 0.1)
    (planet (->Vector -440 -200) 150 10 :green_planet1 0.1)
    (planet (->Vector 2300 -100) 100 9 :green_planet1 0.1)
    (planet (->Vector -900 -2240) 200 11 :pink_planet1 0.1)
    (planet (->Vector 1020 -2100) 150 8 :pink_planet1 0.1)
    (planet (->Vector -100 1100) 120 10 :pink_planet1 0.1)
    (asteroid (->Vector -1220 -1232) 50 3 :ast1 0.1)
    (asteroid (->Vector -200 -300) 50 3 :ast2 0.1)
    (asteroid (->Vector -400 -200) 40 3 :ast3 0.1)
    (asteroid (->Vector -200 -1200) 70 3 :ast4 0.1)
    (asteroid (->Vector -2100 -2100) 80 3 :ast5 0.1)
    (asteroid (->Vector 1200 510) 90 3 :ast2 0.1)
    (asteroid (->Vector -920 -700) 70 3 :ast3 0.1)
    (asteroid (->Vector 1000 -1500) 80 3 :ast1 0.1)
    (asteroid (->Vector 1100 -1810) 80 3 :ast1 0.1)
   ]])
