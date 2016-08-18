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

(def sprite-is-visible-by-player?
  (fn [w pid eid] (< (dist (w eid :position)
                           (w pid :position)) 1000)))


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

(defn star [xy mass radius sprite dangle]
  {
   :type :star
   :position xy
   :mass mass
   :phys-act true
   :sprite sprite
   :radius radius
   :dangle dangle
   :visible? sprite-is-visible-by-player?
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
   :visible? sprite-is-visible-by-player?
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
   :visible? sprite-is-visible-by-player?
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
    (ast-sprite :ast1 "ast1.png")
    (ast-sprite :ast2 "ast2.png")
    (ast-sprite :ast3 "ast3.png")
    (ast-sprite :ast4 "ast4.png")
    (ast-sprite :ast5 "ast5.png")
    (ast-sprite :ast6 "ast6.png")

    {:assets
     {:lstars1 {:class :layer
                :parallax 0.99
                :zorder 1}
      :layer/bg1 {:class :layer
                  :parallax 0.8
                  :alpha 0.7
                  :zorder -20}
      :layer/bg2 {:class :layer
                  :parallax 0.95
                  :alpha 0.7
                  :zorder -10}
      }}

    {:assets
     {:stat-sprite/bg {:class :sprite
                       :type :tiling-sprite
                       :texture "/img/back.png"
                       :pivot [2500 2500]
                       :width 5000
                       :height 5000}}
     }

    {:type :bg-sprite
     :layer :layer/bg1
     :position (->Vector 0 0)
     :visible? (constantly true)
     :sprite :stat-sprite/bg}

    {:type :bg-sprite
     :layer :layer/bg2
     :position (->Vector 0 0)
     :visible? (constantly true)
     :sprite :stat-sprite/bg}

    {:class :sprite
     :type :animator
     :layer :lstars1
     :a-scale {:kind :sin :period 5000 :min 0.9 :max 1.1 :power 2}
     :a-rotation {:kind :saw :period 3140 :min 0 :max 630}
     :child {:type :container
             :children [{:type :animator
                         :a-rotation {:kind :sin :min 0 :max 63 :period 10000}
                         :child {:type :sprite :texture "/img/black1.png" :anchor [0.5 0.5]}}]}}

    {:type :test-sprite
     :position (->Vector 0 0)
     :visible? (constantly true)
     ;; :layer :layer/bg1
     :sprite {:class :sprite
              :type :animator
              :a-rotation {:kind :sin :period 300000 :min 10 :max 20}
              :rotation 4
              :child {:class :sprite
                      :type :random-tilemap
                      :position [0 0]
                      :alpha 0.3
                      :tile-size [64 64]
                      :tile-bounds [[-20 -20] [20 20]]
                      :tiles [:ast1 :ast2 :ast3 :ast4 :ast5 :ast6]
                      }}}

    {:assets
     {:bullet {:class :sprite
               :type :animator
               :shift :start
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
      :black-hole {:class :sprite
                   :type :animator
                   :layer :lstars1
                   :a-scale {:kind :sin :period 5000 :min 0.9 :max 1.1 :power 2}
                   :a-rotation {:kind :saw :period 3140 :min 0 :max 630}
                   :child {:type :container
                           :children [{:type :animator
                                       :a-rotation {:kind :sin :min 0 :max 63 :period 10000}
                                       :child {:type :sprite :texture "/img/black1.png" :anchor [0.5 0.5]}}]}}
      }}

    (black-hole (->Vector 0 0) 0.2)
    (star (->Vector -2100 -1350) 500 33 :star1 0.1)
    (star (->Vector 1200 500) 2000 20 :star2 0.1)
    (star (->Vector -900 -700) 1000 66 :star3 0.1)
    (star (->Vector 400 300) 175 70 :star2 0.1)
    (star (->Vector -1600 1000) 1750 70 :star3 0.1)

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
