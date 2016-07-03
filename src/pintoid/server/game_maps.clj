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
   :sprite :racket-red
   })


(def bullet-proto
  {:type :bullet
   :sprite :bullet
   :fxy (->Vector 0 0)
   :position (->Vector 0 0)
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
   :sprite :ast6
   :fxy (->Vector 0 0)
   :position (->Vector 0 0)
   :phys-move true
   :phys-act true
   :mass 2000
   :angle 0
   :radius 10
   :bullet {:lifetime 10000
            :cooldown 1200
            :velocity 2.1}
   })

(defn star [xy mass radius sprite dangle]
  {
   :type :star
   :position xy
   :mass mass
   :phys-act true
   :sprite sprite
   :radius radius
   :dangle dangle
   })

(defn planet [xy mass radius sprite dangle]
  {
   :type :planet
   :position xy
   :mass mass
   :phys-act true
   :sprite sprite
   :radius radius
   :dangle dangle
   })

(defn asteroid [xy mass radius sprite dangle]
  {
   :type :ast
   :position xy
   :mass mass
   :phys-move true
   :sprite sprite
   :radius radius
   :dangle dangle
   })

(defn black-hole [xy dangle]
  {
   :type :black
   :position xy
   :mass 5000
   :phys-act true
   :sprite :black-hole
   :radius 1
   :dangle dangle
   })

(defn simple-sprite [id image]
  {:sprite-proto
   {id {:type :sprite
         :anchor [0.5 0.5]
         :texture (str "/img/" image)}}})

(defn texture [id image]
  {:texture-info
   {id {:image (str "/img/" image)}}})

(def game-maps
  [[
    (texture :racket-red "racket_red.png")
    (texture :racket-blue "racket_blue.png")

    (simple-sprite :racket-blue "racket_blue.png")
    (simple-sprite :racket-red "racket_red.png")
    (simple-sprite :star1 "star1.png")
    (simple-sprite :star2 "star2.png")
    (simple-sprite :star3 "star3.png")
    (simple-sprite :planet1 "pink_planet1.png")
    (simple-sprite :planet2 "green_planet1.png")
    (simple-sprite :ast1 "ast1.png")
    (simple-sprite :ast2 "ast2.png")
    (simple-sprite :ast3 "ast3.png")
    (simple-sprite :ast4 "ast4.png")
    (simple-sprite :ast5 "ast5.png")
    (simple-sprite :ast6 "ast6.png")

    {:sprite-proto
     {:bullet {:type :animator
               :rand-shift true
               :a-rotation {:kind :saw :period 200 :min 0 :max 6.3}
               :a-scale {:kind :sin :period 500 :min 0.1 :max 0.8}
               :child {:type :container
                       :children [{:type :sprite
                                   :anchor [0.5 0.5]
                                   :scale 0.4
                                   :texture "/img/ast6.png"
                                   :position [0 30]}
                                  {:type :sprite
                                   :anchor [0.5 0.5]
                                   :scale 0.4
                                   :texture "/img/ast6.png"
                                   :position [-20 -8]}
                                  {:type :sprite
                                   :anchor [0.5 0.5]
                                   :scale 0.4
                                   :texture "/img/ast6.png"
                                   :position [20 -8]}]}}
      :black-hole {:type :animator
                   :a-scale {:kind :sin :period 5000 :min 0.9 :max 1.1 :power 2}
                   :a-rotation {:kind :saw :period 3140 :min 0 :max 6300}
                   :child {:type :sprite
                           :texture "/img/black1.png"
                           :anchor [0.5 0.5]}
                   }
      }}

    (black-hole (->Vector 0 0) 0.2)
    (star (->Vector -2100 -1350) 500 33 :star1 0.1)
    (star (->Vector 1200 500) 2000 20 :star2 0.1)
    (star (->Vector -900 -700) 1000 66 :star3 0.1)
    (star (->Vector 400 300) 175 70 :star4 0.1)
    (star (->Vector -1600 1000) 1750 70 :star4 0.1)

    (planet (->Vector -2440 -900) 150 10 :planet1 0.1)
    (planet (->Vector 10 -100) 100 9 :planet1 0.1)
    (planet (->Vector 900 -2140) 200 11 :planet2 0.1)
    (planet (->Vector -900 -2240) 200 11 :planet1 0.1)

    (asteroid (->Vector -1220 -1232) 50 3 :ast1 0.1)
    (asteroid (->Vector -200 -300) 50 3 :ast2 0.1)
    (asteroid (->Vector -920 -700) 70 3 :ast3 0.1)
    (asteroid (->Vector 1900 1500) 80 3 :ast4 0.1)
    (asteroid (->Vector 2100 -1110) 80 3 :ast1 0.1)
    (asteroid (->Vector -1920 -1232) 50 3 :ast1 0.1)
    (asteroid (->Vector -200 -300) 50 3 :ast2 0.1)
    (asteroid (->Vector -320 -710) 70 3 :ast3 0.1)
    (asteroid (->Vector 100 -1000) 80 3 :ast4 0.1)
    (asteroid (->Vector 110 -310) 80 3 :ast1 0.1)

   ]])
