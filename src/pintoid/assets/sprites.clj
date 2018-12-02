(ns pintoid.assets.sprites
  (:use pintoid.ecs.entity)
  (:require [pintoid.server.vec2 :as v2]))

(defasset lstars1 :layer {:parallax 1.00 :zorder 80})

;; Hud
(defasset hud-scores :layer
  {:zorder 100})

(defasset player-score :sprite
  {:type :text
   :anchor [0.5 2.5]
   :layer hud-scores
   :angle 0
   :style {"fill" "silver" "fontFamily" "Arial" "fontSize" "20px"}})

(defn rotate [s period]
  {:type :animator
   :shift :random
   :a-rotation {:kind :saw :period period :min 0 :max 6.3}
   :child s})

(defn pulsate [s period range]
  {:type :animator
   :shift :random
   :a-scale {:kind :sin :period period :min (- 1 range) :max (+ 1 range)}
   :child s})

(defn- mkas [image & {:as opts}]
  (assoc opts :anchor [0.5 0.5] :texture (str "/img/" image)))

(defasset racket-blue :sprite
  (mkas "racket_blue.png"))

(defasset racket-red :sprite
  (mkas "racket_red.png"))

(defasset star1 :sprite
  (->
   (mkas "star1.png")
   (rotate 25000)
   (pulsate 9000 0.08)
   ))

(defasset star2 :sprite
  (->
   (mkas "star2.png")
   (rotate 10000)
   (pulsate 3000 0.01)
   ))

(defasset star3 :sprite
  (->
   (mkas "star3.png")
   (rotate 100000)
   (rotate 10000)
   ))

(defasset star4 :sprite
  (->
   (mkas "star4.png")
   (rotate 100000)
   (rotate 10000)
   ))

(defasset planet1 :sprite
  (-> (mkas "pink_planet1.png")
      (assoc :anchor [0.45 0.5])
   ))

(defasset planet2 :sprite
  (->
   (mkas "green_planet1.png")
   ))

(defassets ast-static-sprites :sprite
  [i (range 1 5)]
  {:anchor [0.5 0.5]
   :texture (str "/img/ast" i ".png")})

(defassets ast-sprites :sprite
  [x (range 1 35)]
  {:type :animator
   :scale 0.6
   :a-rotation {:kind :saw
                :period (* (+ 0.1 (rand)) 10000)
                :min 0
                :max (rand-nth [-6.3 6.3])}
   :child (rand-nth ast-static-sprites)})

(defasset bullet-texture :texture
  {:image "/img/bullet.png"})

(defasset bullet-alt-texture :texture
  {:image "/img/bullet-alt.png"})

(defasset bullet-sprite :sprite
  (->
   {:texture bullet-texture
    :anchor [0.5 0.5]
    :alpha 0.8}
   (pulsate 200 0.2)
   (assoc :scale 0.9)
   ))

(defasset bullet-alt-sprite :sprite
  (->
   {:texture bullet-alt-texture
    :anchor [0.5 0.5]
    :alpha 0.9}
   (pulsate 1222 0.5)
   (pulsate 1333 0.5)
   (pulsate 1700 0.5)
   (assoc :scale 0.5)
   ))

(defasset blackhole-texture :texture
  {:image "/img/black1.png" :anchor [0.5 0.5]})

(defasset blackhole-sprite :sprite
  (-> {:class :sprite :texture "/img/black1.png"}
      (assoc :anchor [0.5 0.5])
      (rotate 100)
      (rotate 233)
      (rotate 750)
      (pulsate 500 0.3)
      (assoc :layer lstars1)))

