(ns pintoid.client.uinput
  (:use [pintoid.client.animation :only
         [add-action!
          defer-action!]])
  (:require-macros
   [pintoid.client.utils :refer [log]]
   ))

      
(def active-keys (array))

(defn handle-keydown [e]
  (let [
    evt (or e (.event js/window))
    char-code (or (.-keyCode evt) (.-which evt))]
    (when (< (.indexOf active-keys char-code) 0) 
      (.push active-keys char-code))
      (log :debug "active keys" active-keys)))
      
(defn handle-keyup [e]
  (let [
    evt (or e (.event js/window))
    char-code (or (.-keyCode evt) (.-which evt))
    new-active-keys (.filter active-keys
                             (fn [key] (not= key char-code)) )]
    (set! active-keys new-active-keys)))

(defn init-user-input []
  (.addEventListener js/window "keydown" handle-keydown false)
  (.addEventListener js/window "keyup" handle-keyup false))

(def angle 0)

(defn key-pressed? [char-code] 
  (>= (.indexOf active-keys char-code) 0))

(defn get-user-input-state []
  (let [space 32
        arrow-left 37
        arrow-right 39
        arrow-up 38
        arrow-down 40
        rotate-step (/ (.-PI js/Math) 10)
        engine-dir (cond
                      (key-pressed? arrow-up) 1
                      (key-pressed? arrow-down) -1
                      :else 0)
        ]
    (when (key-pressed? arrow-left)
      (set! angle (- angle rotate-step)))
    (when (key-pressed? arrow-right)
      (set! angle (+ angle rotate-step)))

    {:angle angle
     :engine-dir engine-dir
     :fire? (key-pressed? space)}
    ))
