(ns pintoid.client.core
  (:require [cljsjs.pixi]
            [dommy.core :as d])
  (:use
   [pintoid.client.utils :only [panic!]]
   [pintoid.client.cs-comm :only
    [init-cs-communication
     spawn-user-input-sender]]
   [pintoid.client.animation :only
    [process-animation!
     process-deffered-actions!]]
   [pintoid.client.graphics :only
    [init-pixi-renderer
     render-graphics!]]
   [pintoid.client.uinput :only
    [init-user-input
     get-user-input-state]])
  (:require-macros
   [dommy.core :refer [sel1]]
   [pintoid.client.utils :refer [log]]
   ))

(enable-console-print!)

;; (- server-time client-time)
;; TODO: ping server every 10secs & update
(def client-server-time-diff 0)
(def network-latency 50)

;; should be ~ (/ 2000 server-snapshots-per-second)
(def animation-interpolation-lag 100)

;; --

(defn client-time []
  (.now js/Date))

(defn drawing-loop [_]
  (let [ct (client-time)
        cgt (+ ct client-server-time-diff)
        at (- cgt animation-interpolation-lag network-latency)]
    (log :trace "draw-loop" at)
    (process-animation! at)
    (render-graphics!)
    (process-deffered-actions!))
  (js/requestAnimationFrame drawing-loop))

(defn start-app []
  (init-cs-communication)
  (init-user-input)
  (spawn-user-input-sender get-user-input-state)
  (d/append! (sel1 :body) (init-pixi-renderer))
  (drawing-loop 0))

(set! (.-onload js/window) start-app)
