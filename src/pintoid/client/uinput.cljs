(ns pintoid.client.uinput
  (:use [clojure.set :only [map-invert]])
  (:require-macros
   [pintoid.client.utils :refer [log]]))

(def active-keys (atom #{}))

(def key-to-keycode
  {:space 32
   :arrow-left 37
   :arrow-right 39
   :arrow-up 38
   :arrow-down 40
   :enter 13})

(def keycode-to-key
  (map-invert key-to-keycode))

(defn handle-keypress [e]
  (let [evt (or e (.event js/window))
        key-code (or (.-keyCode evt) (.-which evt))
        key (keycode-to-key key-code)
        type (.-type e)]
    (when key
      (case type
        "keydown" (swap! active-keys conj key)
        "keyup" (swap! active-keys disj key)))))

(defn init-user-input []
  (.addEventListener js/window "keydown" handle-keypress false)
  (.addEventListener js/window "keyup" handle-keypress false))

(defn key-pressed? [key]
  (@active-keys key))

(defn get-user-input-state []
  (let [ac @active-keys
        tc1 #(if (ac %1) %2 0)
        engine-dir (+ (tc1 :arrow-up 1)(tc1 :arrow-down -1))
        rotate-dir (+ (tc1 :arrow-left -1)(tc1 :arrow-right 1))]
    {:rotate-dir rotate-dir
     :engine-dir engine-dir
     :fire? (ac :space)
     :alt-fire? (ac :enter)}))
