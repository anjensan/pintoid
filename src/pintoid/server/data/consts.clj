(ns pintoid.server.data.consts
  (:use [pintoid.server.ecs core entity])
  (:require [pintoid.server.vec2 :as v2]))

(def world-radius 5000)
(def world-kill-radius 12000)
(def world-jelly-radius 5000)
(def world-jelly-coef 0.01)

(def gravity-g 0.015)

(def engine-forward-force 0.08)
(def engine-reverse-force 0.02)
(def rotate-speed 0.21)
(def max-user-view-distance 2500)
(def max-object-velocity 2)
