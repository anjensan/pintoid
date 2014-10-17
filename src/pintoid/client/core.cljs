(ns pintoid.client.core

  (:use
   [pintoid.client.utils :only [panic!]]
   [pintoid.client.cs-comm :only
    [init-cs-communication
     send-message-to-server
     handle-server-message]]
   [pintoid.client.engine :only [world]]
   [pintoid.client.animation :only
    [process-animation!
     process-deffered-actions!]]
   [pintoid.client.graphics :only
    [init-pixi-renderer
     render-graphics!
     ]]
   [pintoid.client.uinput :only
    [init-user-input]])

  (:require [dommy.core :as d])
  (:require-macros
   [dommy.core :refer [sel1]]))


(enable-console-print!)

;; (- server-time client-time)
;; TODO: ping server every 10secs & update
(def client-server-time-diff 0)
(def network-latency 50)

;; should be ~ (/ 2000 server-snapshots-per-second)
(def animation-interpolation-lag 250)

;; --

(defn client-time [] (.now js/Date))

(defn game-time [] (:at @world))

(defn drawing-loop [_]
  (let [gt (game-time)
        ct (client-time)
        cgt (+ ct client-server-time-diff)
        at (- cgt animation-interpolation-lag network-latency)]
    (process-animation! at)
    (render-graphics!)
    (process-deffered-actions!))
  (js/requestAnimationFrame drawing-loop))


(defn start-app []
  (init-cs-communication)
  (init-user-input)
  (d/append! (sel1 :body) (init-pixi-renderer))
  (drawing-loop 0))


(set! (.-onload js/window) start-app)
