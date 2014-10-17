(ns pintoid.client.core
  (:require
   [chord.client :refer [ws-ch]]
   [cljs.core.async :refer [<! >! close! timeout]]
   [dommy.core :as d])
  (:require-macros
   [dommy.core :refer [sel1]]
   [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)


(defn fire-app-panic [msg]
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


(defn drawing-loop [_]
  ;; TODO
  (js/requestAnimationFrame drawing-loop))


(defn start-drawing-loop []
  (drawing-loop 0))


(defn window-on-load []
  (js/alert "pindroid loaded")
  (init-server-communication)
  (init-keybindings)
  (start-drawing-loop))


(set! (.-onload js/window) window-on-load)

