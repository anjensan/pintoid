(ns pintoid.client.uinput
  (:require
   [pintoid.client.graphics]
   [pintoid.client.layer]
   [taoensso.timbre :as timbre]
   [pintoid.client.utils :refer [point->vec]]
   [pintoid.client.engine :refer [world]]
   [pintoid.client.ceh :refer [player-entity]]
   [clojure.set :refer [map-invert]]))

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
    (case type
      "keydown" (swap! active-keys conj key)
      "keyup" (swap! active-keys disj key))))

(defn init-user-input []
  (.addEventListener js/window "keydown" handle-keypress false)
  (.addEventListener js/window "keyup" handle-keypress false))

(defn key-pressed? [key]
  (@active-keys key))

(defn- get-global-mouse []
  (point->vec (.. pintoid.client.graphics/pixi-renderer -plugins -interaction -mouse -global)))

(defn get-user-input-state []
  (let [[mx my] (pintoid.client.layer/to-game-coords (get-global-mouse))
        [px py] (:position (player-entity @world))
        a (js/Math.atan2 (- my py) (- mx px))
        ac @active-keys
        tc1 #(if (ac %1) %2 0)]
    {:engine-dir (+ (tc1 :arrow-up 1) (tc1 :arrow-down -1))
     :rotate a
     :fire? (ac :space)
     :alt-fire? (ac :enter)}))
