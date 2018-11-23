(ns pintoid.assets.gameworld
  (:use [pintoid.ecs.entity]
        [pintoid.assets proto sprites])
  (:require [pintoid.server.vec2 :as v2]))

(defn- rand-pos [x]
  (let [w 4500
        w2 (* 2 w)
        h (hash x)
        f #(-> % (* h) (rem w2) (- w))]
    (v2/vec2 (f 11) (f 13))))

(defentity center-bh
  (blackhole :position v2/zero))

(defentity star-1
  (star :position (v2/vec2 -2100 -1350)
        :mass 1500
        :radius 33
        :sprite star1))

(defentity star-2
  (star :position (v2/vec2 1200 500)
        :mass 5000
        :radius 20
        :sprite star2))

(defentity star-3
  (star :position (v2/vec2 -900 -700)
        :mass 2000
        :radius 66
        :sprite star3))

(defentity star-4
  (star :position (v2/vec2 400 300)
        :mass 1500
        :radius 70
        :sprite star2))

(defentity star-5
  (star :position (v2/vec2 -1600 1000)
        :mass 1750
        :radius 70
        :sprite star3))

(defentity planet-1
  (planet :position (v2/vec2 -2440 -900)
          :mass 850
          :radius 10
          :sprite planet1))

(defentity planet-2
  (planet :position (v2/vec2 100 -900)
          :mass 900
          :radius 9
          :sprite planet1))

(defentity planet-3
  (planet :position (v2/vec2 900 -2140)
          :mass 900
          :radius 11
          :sprite planet2))

(defentity planet-4
  (planet :position (v2/vec2 -900 -2240)
          :mass 300
          :radius 11
          :sprite planet1))

(defentities asteroids
  [[m c] [[50 30]
          [80 20]
          [100 15]
          [150 10]
          [200 5]]
   x (range c)]
  (asteroid :position (rand-pos (hash [m x]))
            :mass m
            :radius 3
            :sprite (rand-nth ast-sprites)))
