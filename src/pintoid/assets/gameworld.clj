(ns pintoid.assets.gameworld
  (:use [pintoid.ecs.entity]
        [pintoid.assets proto sprites])
  (:require [pintoid.server.vec2 :as v2]))

(defn- rand-pos [x]
  (let [w 4500
        h (hash x)
        f #(-> % (* h) (rem w))]
    (v2/vec2 (f 11) (f 13))))

(defentity center-bh
  (blackhole :position v2/zero
             :mass 25000
             ))

(defentity star-1
  (star :position (v2/vec2 2000 900)
        :mass 3500
        :radius 65
        :sprite star1))

(defentity star-2
  (star :position (v2/vec2 100 -800)
        :mass 7000
        :radius 55
        :sprite star2))

(defentity star-3
  (star :position (v2/vec2 -3000 100)
        :mass 9000
        :radius 115
        :sprite star3))

(defentity star-4
  (star :position (v2/vec2 800 -3000)
        :mass 5000
        :radius 90
        :sprite star4))

(defentity planet-1
  (planet :position (v2/vec2 700 -2400)
          :mass 850
          :radius 25
          :sprite planet1))

(defentity planet-3
  (planet :position (v2/vec2 -3000 -400)
          :mass 900
          :radius 40
          :sprite planet2))

(defentities asteroids
  [[i m c] [[0 10 20]
            [1 25 20]
            [2 50 15]
            [3 70 10]
            [4 150 5]]
   x (range c)]
  (asteroid :position (rand-pos (hash [m x]))
            :mass m
            :sprite (rand-nth ast-sprites)))
