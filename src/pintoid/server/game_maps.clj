(ns pintoid.server.game-maps
  (:use pintoid.server.math))

(def world-height 10000)
(def world-width 10000)

(def gravity-g 0.005)
(def engine-forward-force 0.12)
(def engine-reverse-force 0.05)
(def rotate-speed 0.15)

(def max-user-view-distance 2500)

(defn calc-gravity-force [^double m1 ^double m2 p1 p2]
  (let [d (dist p1 p2)]
    (if (zero? d)
      (->Vector 0 0)  ; FIXME: remove this?
      (let [r' (/ d)
            fx (* gravity-g r' m1 r' m2 r')]
        (vs* (v- p2 p1) fx)))))

(defn visible-by-player? [w pid eid]
  (< (dist (w eid :position)
           (w pid :position)) max-user-view-distance))

(def ^{:doc "Required attribute"} ? :?)


(defn- ensure-no-missing-fields [es]
  (doseq [e es]
    (doseq [[k v] e]
      (assert (not= v ?))))
  es)


(defn v
  ([[x y]] (->Vector x y))
  ([x y] (->Vector x y)))


;; Game entities prototypes

(def player
  {:player true
   :type :player
   :position ?
   :fxy (v 0 0)
   :phys-move true
   :mass 100
   :angle 0
   :radius 30
   :sprite :sprite/racket-red
   :visible? (constantly true)
   })


(def bullet
  {:type :bullet
   :sprite :sprite/bullet
   :fxy (v 0 0)
   :position (v 0 0)
   :phys-move true
   :mass 50
   :angle 0
   :radius 20
   :bullet {:lifetime 5000
            :cooldown 200
            :velocity 1.0}
   :visible? visible-by-player?
   })

(def bullet-alt
  {:type :bullet
   :sprite :sprite/ast6
   :fxy (v 0 0)
   :position (v 0 0)
   :phys-move true
   :phys-act true
   :mass 2000
   :angle 0
   :radius 10
   :bullet {:lifetime 10000 :cooldown 1200 :velocity 2.1}
   :visible? visible-by-player?
   })

(def star
  {:type :star
   :position ?
   :mass ?
   :sprite ?
   :radius ?
   :phys-act true
   :layer :layer/lstars1
   :visible? visible-by-player?
   })

(def planet
  {:type :planet
   :position ?
   :mass ?
   :sprite ?
   :radius ?
   :phys-act true
   :visible? visible-by-player?
   })

(def asteroid
  {:type :ast
   :position ?
   :sprite ?
   :mass 50
   :radius 5
   :phys-move true
   :visible? visible-by-player?
   })

(def blackhole
  {:type :black
   :position ?
   :mass 5000
   :phys-act true
   :sprite :sprite/black-hole
   :radius 2
   :visible? visible-by-player?
   })


;; Helpers

(defn- parse-props-and-child [pas]
  [(into {} (partition 2 pas))
   (when (odd? (count pas)) (last pas))])


(defmacro ^:private defassethelper
  ([nam dfs]
   (defassethelper nam dfs false))
  ([nam dfs child?]
   `(defn- ~nam [& pas#]
      (when (and (not ~child?) (odd? (count pas#)))
        (throw (ex-info "Asset doesn't support child"
                        {:params pas#, :name '~nam})))
      (let [p# (partition 2 pas#)
            r# (into ~dfs (map vec) p#)]
        (if (odd? (count pas#))
          (assoc r# :child (last pas#))
          r#)))))

(defassethelper sprite {:class :sprite} true)
(defassethelper animator {:class :sprite :type :animator} true)
(defassethelper container {:class :sprite :type :container} true)
(defassethelper random-tilemap {:class :sprite :type :random-tilemap} false)
(defassethelper texture {:class :texture} false)
(defassethelper layer {:class :layer} false)


(defn- assets [& as]
  (for [[i a] (partition-all 2 as)]
    {:assets {i a}}))


;; === Assets & game entitites

(defn stars-sky []
  [
   ;; Layers
   (assets
    :layer/starsky0 (layer :parallax 0.70, :zorder -990)
    :layer/starsky1 (layer :parallax 0.85, :zorder -980)
    :layer/starsky2 (layer :parallax 0.95, :zorder -970)
    :layer/starsky3 (layer :parallax 1.15, :zorder 100))

   ;; Tiles
   (assets :texture/starsky (texture :image "/img/starsky.jpeg"))
   (for [c (range 8), r (range 4),
         :let [x (str "starsky-" c "x" r)]]
     (assets
      (keyword "texture" x) (texture :base :texture/starsky
                                     :frame {:x (* 512 c) :y (* 512 r) :w 512 :h 512})
      (keyword "sprite" x) (sprite
                            :blend-mode :add
                            :texture (keyword "texture" x))))

   (for [[i [a s]] (map vector (range)
                        [[0.5 0.5] [0.65 0.75] [0.85 1] [0.10 1.8]])
         :let [x (keyword "sprite" (str "starsky-layer-sprite-" i))]]
     [(assets
       x (random-tilemap
          :hash-seed (str "seed" i)
          :tile-size [512 512]
          :tile-group [2 2]
          :alpha a
          :scale s
          :tiles (vec (for [c (range 8), r (range 4)]
                        (keyword "sprite" (str "starsky-" c "x" r))))))
      {:type :starsky-sprite
       :position (v (* 10000 a) (* 1000 i))
       :visible? (constantly true)
       :layer (keyword "layer" (str "starsky" i))
       :sprite x}])

   ])


(defn commmon-assets []
  [
   ;; Layers
   (assets :layer/lstars1 (layer :zorder 80))

   ;; Textures
   (assets
    :texture/racket-red (texture :image "/img/racket_red.png")
    :texture/racket-blue (texture :image "/img/racket_blue.png"))

   ;; Hud
   (assets
    :layer/hud-scores (layer :zorder 100)
    :sprite/player-score (sprite :type :text, :anchor [0.5 2.5],
                                 :style {"fill" "white" "font" "normal 18px Arial"}))

   ;; Sprites
   (letfn [(mkas [image]
             (sprite :anchor [0.5 0.5] :texture (str "/img/" image)))]
     (assets
      :sprite/racket-blue (mkas "racket_blue.png")
      :sprite/racket-red (mkas "racket_red.png")
      :sprite/star1 (mkas "star1.png")
      :sprite/star2 (mkas "star2.png")
      :sprite/star3 (mkas "star3.png")
      :sprite/planet1 (mkas "pink_planet1.png")
      :sprite/planet2 (mkas "green_planet1.png")))

   (letfn [(mkas [image]
             (animator
              :a-rotation {:kind :saw :period 31400 :min 0 :max 630}
              (sprite :anchor [0.5 0.5], :texture (str "/img/" image))))]
     (assets
      :sprite/ast1 (mkas "ast1.png")
      :sprite/ast2 (mkas "ast2.png")
      :sprite/ast3 (mkas "ast3.png")
      :sprite/ast4 (mkas "ast4.png")
      :sprite/ast5 (mkas "ast5.png")
      :sprite/ast6 (mkas "ast6.png")
      :texture/ast6 (texture :image "/img/ast6.png")
      ))

   (assets
    :sprite/bullet
    (animator
     :shift :start
     :a-rotation {:kind :saw :period 200 :min 0 :max 6.3}
     :a-scale {:kind :sin :period 300 :min 0.2 :max 0.4}
     [(sprite :texture :texture/ast6, :anchor [0.5 0.5], :scale 0.4, :position [0 30])
      (sprite :texture :texture/ast6, :anchor [0.5 0.5], :scale 0.4, :position [-20 -8])
      (sprite :texture :texture/ast6, :anchor [0.5 0.5], :scale 0.4, :position [20 -8])])

    :sprite/black-hole
    (animator
     :layer :layer/lstars1
     :a-scale {:kind :sin :period 5000 :min 0.8 :max 1.2 :power 1.5}
     :a-rotation {:kind :sin :period 3777 :min 0 :max 3.1415}
     (animator
      :a-rotation {:kind :saw :min 0 :max 314.1592 :period 10000}
      (sprite :texture "/img/black1.png" :anchor [0.5 0.5]))))
   ])


(defn game-entities []
  [(assoc blackhole :position (v 0 0))
   (assoc star :position (v -2100 -1350), :mass 500, :radius 33, :sprite :sprite/star1)
   (assoc star :position (v 1200 500), :mass 2000, :radius 20, :sprite :sprite/star2)
   (assoc star :position (v -900 -700), :mass 1000, :radius 66, :sprite :sprite/star3)
   (assoc star :position (v 400 300), :mass 175, :radius 70, :sprite :sprite/star2)
   (assoc star :position (v -1600 1000), :mass 1750, :radius 70, :sprite :sprite/star3)
   (assoc planet :position (v -2440 -900), :mass 150, :radius 10, :sprite :sprite/planet1)
   (assoc planet :position (v 10 -100), :mass 100, :radius 9, :sprite :sprite/planet1)
   (assoc planet :position (v 900 -2140), :mass 200, :radius 11, :sprite :sprite/planet2)
   (assoc planet :position (v -900 -2240), :mass 200, :radius 11, :sprite :sprite/planet1)
   (assoc asteroid :position (v -1220 -1232), :mass 50, :radius 3, :sprite :sprite/ast1)
   (assoc asteroid :position (v -200 -300), :mass 50, :radius 3, :sprite :sprite/ast2)
   (assoc asteroid :position (v -920 -700), :mass 70, :radius 3, :sprite :sprite/ast3)
   (assoc asteroid :position (v 1900 1500), :mass 80, :radius 3, :sprite :sprite/ast4)
   (assoc asteroid :position (v 2100 -1110), :mass 80, :radius 3, :sprite :sprite/ast1)
   (assoc asteroid :position (v -1920 -1232), :mass 50, :radius 3, :sprite :sprite/ast1)
   (assoc asteroid :position (v -200 -300), :mass 50, :radius 3, :sprite :sprite/ast2)
   (assoc asteroid :position (v -320 -710), :mass 70, :radius 3, :sprite :sprite/ast3)
   ])


(defn game []
  (-> []
      (concat (commmon-assets))
      (concat (stars-sky))
      (concat (game-entities))
      (flatten)
      (ensure-no-missing-fields)))
