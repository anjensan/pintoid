(ns pintoid.client.core
  (:require [cljsjs.pixi]
            [dommy.core :as d]
            [pintoid.client.graphics.core :as g]
            [pintoid.client.graphics.animloop :as al]
            [taoensso.timbre :as timbre :include-macros true])
  (:use
   [pintoid.client.utils :only [panic!]]
   [pintoid.client.cswiring :only
    [init-cs-communication
     spawn-user-input-sender
     send-message-to-server
     client-server-time-diff]]
   [pintoid.client.uinput :only
    [init-user-input
     get-user-input-state]])
  (:require-macros
   [dommy.core :refer [sel1]]
   ))


(timbre/set-level! :info)

;; TODO: implement adaptive interpolation lag (based on ping).
(def animation-interpolation-lag 100)


(defn drawing-loop [timestamp]
  (js/requestAnimationFrame drawing-loop)
  (let [server-timestamp (+ timestamp client-server-time-diff)
        draw-at (- server-timestamp animation-interpolation-lag)]
    (timbre/tracef "Run draw loop at %d" draw-at)
    (al/run-animations! draw-at)
    (g/render-graphics!)
    (js/setTimeout al/run-deffered-actions! 0)))


(defn start-app []
  (init-cs-communication)
  (init-user-input)
  (let [c (g/init-pixi-renderer 3200 1800)]
    (d/append! (sel1 :body) c)
    (g/init-autoscaling c))
  (g/init-pixi-labels)
  (g/update-player-death! 0)
  (g/update-player-score! 0)
  (spawn-user-input-sender get-user-input-state)
  (drawing-loop 0))


(set! (.-onload js/window) start-app)
