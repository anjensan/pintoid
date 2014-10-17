(ns pintoid.client.uinput
  (:use [pintoid.client.engine :only
         [world]]
        [pintoid.client.animation :only
         [add-action!
          defer-action!
          ]]))


(defn handle-keydown [e]
  (case (-.keyCode e)
    37 (println "pressed left")
    38 (println "pressed up")
    39 (println "pressed right")
    40 (println "pressed down")))
      

(defn init-user-input []
  (.addEventListener js/window "keydown" handle-keydown true))
