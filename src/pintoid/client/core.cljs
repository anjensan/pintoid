(ns pintoid.client.core

  (:use [pintoid.client.engine :only
         [world]]
        [pintoid.client.animation :only
         [process-animation!
          process-deffered-actions!]]
        [pintoid.client.graphics :only
         [init-pixi-renderer
          render-graphics!
          ]])

  (:require
   [chord.client :refer [ws-ch]]
   [cljs.core.async :refer [<! >! close! timeout]]
   [dommy.core :as d])

  (:require-macros
   [dommy.core :refer [sel1]]
   [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)


;; global game constatns

;; (- server-time client-time)
;; TODO: ping server every 10secs & update
(def client-server-time-diff 0)
(def network-latency 50)

;; should be ~ (/ 2000 server-snapshots-per-second)
(def animation-interpolation-lag 250)

;; client <> server websocket
(def server-ws-channel nil)


(defn panic-app [msg]
  ;; TODO: stop drawing-loop
  (d/replace-contents!
   (sel1 :#content)
   [:div
    "Something goes wrong! Sorry :'("
    (pr-str msg)]))


(defn receive-server-messages [ws-chan]
  ;; TODO
  )


(defn send-message-to-server [msg]
  ;; TODO
  )


(defn init-server-communication []
  ;; TODO
  )

(defn handle-keydown [e]
  (case (-.keyCode e)
    37 (println "pressed left")
    38 (println "pressed up")
    39 (println "pressed right")
    40 (println "pressed down")))
      

(defn init-keybindings []
  (.addEventListener js/window "keydown" handle-keydown true))


(defn client-time []
  (.now js/Date))


(defn game-time []
  (:at @world))


(defn drawing-loop [_]
  (let [gt (game-time)
        ct (client-time)
        cgt (+ ct client-server-time-diff)
        at (- cgt animation-interpolation-lag network-latency)]
    (process-animation! at)
    (render-graphics!)
    (process-deffered-actions!))
  (js/requestAnimationFrame drawing-loop))


(defn start-drawing-loop []
  (d/append! (sel1 :body) (init-pixi-renderer))
  (drawing-loop 0))


(defn window-on-load []
  (js/alert "pindroid loaded")
  (init-server-communication)
  (init-keybindings)
  (start-drawing-loop))


(set! (.-onload js/window) window-on-load)

