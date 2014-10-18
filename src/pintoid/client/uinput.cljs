(ns pintoid.client.uinput
  (:use [pintoid.client.engine :only
         [world]]
        [pintoid.client.animation :only
         [add-action!
          defer-action!
          ]])
  (:require-macros
   [pintoid.client.utils :refer [log]]
   ))


      
(def active-keys (array))


(defn handle-keydown [e]
  (let [
    evt (or e (.event js/window))
    char-code (or (.-keyCode evt) (.-which evt))
    ]
    (when (< (.indexOf active-keys char-code) 0) 
      (.push active-keys char-code))
      (println active-keys)
    ))
  ; (log :debug "key event" e)
  ; (case (.-keyCode e)
  ;   37 (println "pressed left")
  ;   38 (println "pressed up")
  ;   39 (println "pressed right")
  ;   40 (println "pressed down")
  ;   nil))
      
(defn handle-keyup [e]
  (let [
    evt (or e (.event js/window))
    char-code (or (.-keyCode evt) (.-which evt))
    new-active-keys (.filter active-keys (fn [key] (not= key char-code)) )
    ]
    (set! active-keys new-active-keys)
    )
)
  ; (log :debug "key event" e)
  ; (case (.-keyCode e)
  ;   37 (println "pressed left")
  ;   38 (println "pressed up")
  ;   39 (println "pressed right")
  ;   40 (println "pressed down")
  ;   nil))

(defn init-user-input []
      (.addEventListener js/window "keydown" handle-keydown false)
      (.addEventListener js/window "keyup" handle-keyup false)
      (fn [] active-keys)
)
(def angle 0)

(defn key-pressed? [char-code] 
  (let [
      f (>= (.indexOf active-keys char-code) 0)
      ; _ (println f)
    ]
  f

))

(defn user-input-state []
  (let [
      space 32
      arrow-left 37
      arrow-right 39
      rotate-step (/ (.-PI js/Math) 30)
    ]
    (when (key-pressed? arrow-left) (set! angle (- angle rotate-step)))
    (when (key-pressed? arrow-right) (set! angle (+ angle rotate-step)))
    { :angle angle
      :engine-one?: (key-pressed? space) 
    }
    ))