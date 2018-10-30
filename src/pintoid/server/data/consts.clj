(ns pintoid.server.data.consts
  (:use [pintoid.server.ecs core entity])
  (:require [pintoid.server.vec2 :as v2]))

(def world-radius 4000)
(def gravity-g 0.015)
(def engine-forward-force 0.08)
(def engine-reverse-force 0.02)
(def rotate-speed 0.21)
(def max-user-view-distance 2500)
