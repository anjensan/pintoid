(ns pintoid.server.data.starsky
  (:use [pintoid.server.entity]
        [pintoid.server.data assets consts])
  (:require [pintoid.server.vec2 :as v2]))

(defassets starsky-layers :layer
  [[p z] [[0.70 -990]
          [0.85 -980]
          [0.95 -970]
          [1.15 100]]]
  {:parallax p
   :zorder z})

(defasset starsky-base-texture :texture
  {:image "/img/starsky.jpeg"})

(defassets starsky-single-title-textures :texture
  [c (range 8) r (range 4)]
  {:base starsky-base-texture
   :frame {:x (* 512 c)
           :y (* 512 r)
           :w 512
           :h 512}})

(defassets starsky-titles :sprite
  [t starsky-single-title-textures]
  {:blend-mode :screen
   :texture t})

(defassets starsky-tilemap-sprites :sprite
  [[a s] [[0.7 0.5]
          [0.85 0.75]
          [0.95 1]
          [0.30 1.8]]]
  {:type :random-tilemap
   :hash-seed (hash [a s])
   :tile-size [512 512]
   :tile-advance 2
   :tile-group [4 4]
   :alpha a
   :scale s
   :tiles starsky-titles})

(defasset static-circle-sprite :sprite
  {:type :graphics
   :do [['lineStyle 5 0x998899 0.6]
        ['drawCircle 0 0 world-radius]
        ['endFill]]})

(defentities starsky-tilemaps
  [[i x l] (map vector (range) starsky-tilemap-sprites starsky-layers)]
  {:type :static
   :position (v2/vec2 (* 97 i) (* 47 i))
   :layer l
   :sprite {nil x}})

(defentity static-circle
  {:type :static
   :position (v2/vec2 0 0)
   :sprite static-circle-sprite})
