(ns pintoid.server.game-maps
  (:use pintoid.server.math))

(def world-height 10000)
(def world-width 10000)

(def gravity-g 0.005)
(def engine-forward-force 0.12)
(def engine-reverse-force 0.05)
(def rotate-speed 0.15)

(def bullet-ahead-time 200)
(def max-user-view-distance 2500)

(defn calc-gravity-force [^double m1 ^double m2 p1 p2]
  (let [d (dist p1 p2)]
    (if (zero? d)
      (->Vector 0 0)  ; FIXME: remove this?
      (let [r' (/ d)
            fx (* gravity-g r' m1 r' m2 r')]
        (vs* (v- p2 p1) fx)))))

(def sprite-is-visible-by-player?
  (fn [w pid eid] (< (dist (w eid :position)
                           (w pid :position)) max-user-view-distance)))


(def player-proto
  {:player true
   :type :player
   :fxy (->Vector 0 0)
   :phys-move true
   :mass 100
   :angle 0
   :radius 30
   :sprite :racket-red
   :visible? (constantly true)
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
   :visible? sprite-is-visible-by-player?
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
   :visible? sprite-is-visible-by-player?
   })

(defn star [xy mass radius sprite]
  {
   :type :star
   :position xy
   :mass mass
   :phys-act true
   :sprite sprite
   :radius radius
   :layer :layer/lstars1
   :visible? sprite-is-visible-by-player?
   })

(defn planet [xy mass radius sprite]
  {
   :type :planet
   :position xy
   :mass mass
   :phys-act true
   :sprite sprite
   :radius radius
   :visible? sprite-is-visible-by-player?
   })

(defn asteroid [xy mass radius sprite]
  {
   :type :ast
   :position xy
   :mass mass
   :phys-move true
   :sprite sprite
   :radius radius
   :visible? sprite-is-visible-by-player?
   })

(defn black-hole [xy]
  {
   :type :black
   :position xy
   :mass 5000
   :phys-act true
   :sprite :black-hole
   :radius 1
   :visible? sprite-is-visible-by-player?
   })

(defn simple-sprite [id image]
  {:assets
   {id {:class :sprite
        :type :sprite
        :anchor [0.5 0.5]
        :texture (str "/img/" image)}}})

(defn ast-sprite [id image]
  {:assets
   {id {:class :sprite
        :type :animator
        :a-rotation {:kind :saw :period 31400 :min 0 :max 630}
        :child {:type :sprite
                :anchor [0.5 0.5]
                :texture (str "/img/" image)}}}}
  )

(defn texture [id image]
  {:assets
   {id {:class :texture
        :image (str "/img/" image)}}})


(defn game-map []
  [
   (texture :racket-red "racket_red.png")
   (texture :racket-blue "racket_blue.png")

   {:assets
    {
     :layer/starsky1 {:class :layer :parallax 0.30 :zorder -990}
     :layer/starsky2 {:class :layer :parallax 0.60 :zorder -980}
     :layer/starsky3 {:class :layer :parallax 0.95 :zorder -970}
     :layer/starsky4 {:class :layer :parallax 1.15 :zorder 100}
     :layer/lstars1 {:class :layer :zorder -100}
     :layer/hud-scores {:class :layer :zorder 100}
     :sprite/player-score {:class :sprite :type :text
                           :anchor [0.5 2.5]
                           :style {"fill" "white"
                                   "font" "normal 18px Arial"}}
     }}

   {:assets
    {:texture/starsky {:class :texture :image "/img/starsky.jpeg"}}}

   {:assets
    (reduce
     conj
     (for [c (range 8), r (range 4), :let [x (str "starsky-" c "x" r)]]
       {(keyword "texture" x)
        {:class :texture :base :texture/starsky
         :frame {:x (* 512 c) :y (* 512 r) :w 512 :h 512}}}))}

   {:assets
    (reduce
     conj
     (for [c (range 8), r (range 4), :let [x (str "starsky-" c "x" r)]]
       {(keyword "sprite" x)
        {:class :sprite :type :sprite :blend-mode :add :texture (keyword "texture" x)}}))}

   {:type :starsky-sprite
    :position (->Vector 200 135)
    :visible? (constantly true)
    :layer :layer/starsky1
    :sprite {nil
             {:class :sprite
              :type :random-tilemap
              :hash-seed "l2"
              :tile-size [512 512]
              :tile-group [2 2]
              :alpha 0.5
              :scale 0.5
              :tiles (vec (for [c (range 8), r (range 4)]
                            (keyword "sprite" (str "starsky-" c "x" r))))}}}

   {:type :starsky-sprite
    :position (->Vector 100 450)
    :visible? (constantly true)
    :layer :layer/starsky2
    :sprite {nil
             {:class :sprite
              :type :random-tilemap
              :hash-seed "l1"
              :tile-size [512 512]
              :tile-group [2 2]
              :alpha 0.65
              :scale 0.75
              :tiles (vec (for [c (range 8), r (range 4)]
                            (keyword "sprite" (str "starsky-" c "x" r))))}}}

   {:type :starsky-sprite
    :position (->Vector 0 0)
    :visible? (constantly true)
    :layer :layer/starsky3
    :sprite {nil
             {:class :sprite
              :type :random-tilemap
              :hash-seed "l3"
              :tile-size [512 512]
              :tile-group [2 2]
              :alpha 0.85
              :tiles (vec (for [c (range 8), r (range 4)]
                            (keyword "sprite" (str "starsky-" c "x" r))))}}}

   {:type :starsky-sprite
    :position (->Vector 0 0)
    :visible? (constantly true)
    :layer :layer/starsky4
    :sprite {nil
             {:class :sprite
              :type :random-tilemap
              :hash-seed "l4"
              :tile-size [512 512]
              :tile-group [2 2]
              :alpha 0.05
              :scale 2
              :tiles (vec (for [c (range 8), r (range 4)]
                            (keyword "sprite" (str "starsky-" c "x" r))))}}}

   (simple-sprite :racket-blue "racket_blue.png")
   (simple-sprite :racket-red "racket_red.png")
   (simple-sprite :star1 "star1.png")
   (simple-sprite :star2 "star2.png")
   (simple-sprite :star3 "star3.png")
   (simple-sprite :planet1 "pink_planet1.png")
   (simple-sprite :planet2 "green_planet1.png")
   (ast-sprite :ast1 "ast1.png")
   (ast-sprite :ast2 "ast2.png")
   (ast-sprite :ast3 "ast3.png")
   (ast-sprite :ast4 "ast4.png")
   (ast-sprite :ast5 "ast5.png")
   (ast-sprite :ast6 "ast6.png")

   {:assets
    {:bullet {:class :sprite
              :type :animator
              :shift :start
              :a-rotation {:kind :saw :period 200 :min 0 :max 6.3}
              :a-scale {:kind :sin :period 300 :min 0.2 :max 0.4}
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
     :black-hole {:class :sprite
                  :type :animator
                  :layer :layer/lstars1
                  :a-scale {:kind :sin :period 5000 :min 0.9 :max 1.1 :power 2}
                  :a-rotation {:kind :saw :period 3140 :min 0 :max 630}
                  :child {:type :container
                          :children [{:type :animator
                                      :a-rotation {:kind :sin :min 0 :max 63 :period 10000}
                                      :child {:type :sprite :texture "/img/black1.png" :anchor [0.5 0.5]}}]}}
     }}

   (black-hole (->Vector 0 0))
   (star (->Vector -2100 -1350) 500 33 :star1)
   (star (->Vector 1200 500) 2000 20 :star2)
   (star (->Vector -900 -700) 1000 66 :star3)
   (star (->Vector 400 300) 175 70 :star2)
   (star (->Vector -1600 1000) 1750 70 :star3)

   (planet (->Vector -2440 -900) 150 10 :planet1)
   (planet (->Vector 10 -100) 100 9 :planet1)
   (planet (->Vector 900 -2140) 200 11 :planet2)
   (planet (->Vector -900 -2240) 200 11 :planet1)

   (asteroid (->Vector -1220 -1232) 50 3 :ast1)
   (asteroid (->Vector -200 -300) 50 3 :ast2)
   (asteroid (->Vector -920 -700) 70 3 :ast3)
   (asteroid (->Vector 1900 1500) 80 3 :ast4)
   (asteroid (->Vector 2100 -1110) 80 3 :ast1)
   (asteroid (->Vector -1920 -1232) 50 3 :ast1)
   (asteroid (->Vector -200 -300) 50 3 :ast2)
   (asteroid (->Vector -320 -710) 70 3 :ast3)
   (asteroid (->Vector 100 -1000) 80 3 :ast4)
   (asteroid (->Vector 110 -310) 80 3 :ast1)

   ])
