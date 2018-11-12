(ns pintoid.server.data.assets
  (:use pintoid.server.entity)
  (:require [pintoid.server.vec2 :as v2]))

(defasset lstars1 :layer {:parallax 1.00 :zorder 80})

;; Hud
(defasset hud-scores :layer
  {:zorder 100})

(defasset player-score :sprite
  {:type :text
   :anchor [0.5 2.5]
   :style {"fill" "white" "fontFamily" "Arial" "fontSize" "18px"}})

(defn- mkas [image & {:as opts}]
  (assoc opts :anchor [0.5 0.5] :texture (str "/img/" image)))

(defasset racket-blue :sprite
  (mkas "racket_blue.png"))

(defasset racket-red :sprite
  (mkas "racket_red.png"))

(defasset star1 :sprite
  (mkas "star1.png"))

(defasset star2 :sprite
  (mkas "star2.png"))

(defasset star3 :sprite
  (mkas "star3.png"))

(defasset planet1 :sprite
  (mkas "pink_planet1.png"))

(defasset planet2 :sprite
  (mkas "green_planet1.png"))

(defassets ast-static-sprites :sprite
  [i (range 1 6)]
  {:anchor [0.5 0.5], :texture (str "/img/ast" i ".png")})

(defassets ast-sprites :sprite
  [x (range 1 30)]
  {:type :animator
   :a-rotation {:kind :saw
                :period (* (+ 0.1 (rand)) 10000)
                :min 0
                :max (rand-nth [-6.3 6.3])}
   :child (rand-nth ast-static-sprites)})

(defasset bullet-texture :texture
  {:image "/img/ast6.png"})

(defasset bullet-sprite :sprite
  (letfn [(f [p] {:class :sprite
                  :texture bullet-texture
                  :anchor [0.5 0.5]
                  :scale 0.4,
                  :position p})]
    {:type :animator
     :shift :start
     :a-rotation {:kind :saw :period 200 :min 0 :max 6.3}
     :a-scale {:kind :sin :period 300 :min 0.2 :max 0.4}
     :child [(f [0 30])
             (f [-20 -8])
             (f [20 -8])]}))

(defasset blackhole-texture :texture
  {:image "/img/black1.png" :anchor [0.5 0.5]})

(defasset blackhole-sprite :sprite
  {:type :animator
   :layer lstars1
   :a-scale {:kind :sin :period 5000 :min 0.8 :max 1.2 :power 1.5}
   :a-rotation {:kind :sin :period 3777 :min 0 :max 3.1415}
   :child {:type :animator
           :class :sprite
           :a-rotation {:kind :saw :min 0 :max 314.1592 :period 10000}
           :child {:class :sprite :texture "/img/black1.png" :anchor [0.5 0.5]}}})
