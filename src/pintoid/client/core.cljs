(ns pintoid.client.core
  (:require [cljsjs.pixi]
            [dommy.core :as d]
            [pintoid.client.animloop :as al]
            [pintoid.client.graphics :as g]
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

(def base-gameview-height 900)
(def base-gameview-width 1600)

;; TODO: implement adaptive interpolation lag (based on ping).
(def animation-interpolation-lag 100)
(def user-input-update-period 50)

(defn drawing-loop [timestamp]
  (js/requestAnimationFrame drawing-loop)
  (let [server-timestamp (+ timestamp client-server-time-diff)
        draw-at (- server-timestamp animation-interpolation-lag)]
    (timbre/tracef "Run draw loop at %d" draw-at)
    (al/run-animations! draw-at)
    (g/render-graphics!)))

(defn start-app []
  (timbre/infof "Start application")
  (init-cs-communication)
  (init-user-input)
  (d/append! (sel1 :body) (g/init-pixi-renderer base-gameview-width base-gameview-height))
  (spawn-user-input-sender get-user-input-state user-input-update-period)
  (drawing-loop 0))

(timbre/set-level! :debug)
(set! (.-onload js/window) start-app)
