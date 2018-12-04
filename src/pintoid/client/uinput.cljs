(ns pintoid.client.uinput
  (:require
   [pintoid.client.graphics]
   [pintoid.client.layer]
   [taoensso.timbre :as timbre]
   [pintoid.client.utils :refer [point->vec]]
   [pintoid.client.engine :refer [world]]
   [pintoid.client.ceh :refer [player-entity]]
   ))

(def active-keys (atom #{}))

(def keyname-to-key
  {"ArrowLeft"  :arrow-left
   "ArrowRight" :arrow-right
   "ArrowUp"    :arrow-up
   "ArrowDown"  :arrow-down
   "Shift"      :shift
   "Enter"      :enter
   " "          :space
   })

(def button-to-key
  {0 :mouse-left
   1 :mouse-middle
   2 :mouse-right
   })

(defn handle-keypress [e]
  (when-let [key (keyname-to-key (.-key e))]
    (case (.-type e)
      "keydown" (swap! active-keys conj key)
      "keyup"   (swap! active-keys disj key))))

(defn handle-mousepress [e]
  (when-let [btn (button-to-key (.-button e))]
    (case (.-type e)
      "mousedown" (swap! active-keys conj btn)
      "mouseup"   (swap! active-keys disj btn))))

(defn init-user-input []
  (let [ael #(.addEventListener js/window %1 %2 false)]
    (ael "keydown" handle-keypress)
    (ael "keyup" handle-keypress)
    (ael "mousedown" handle-mousepress)
    (ael "mouseup" handle-mousepress)
    (ael "contextmenu" #(.preventDefault %)))
  )

(defn- get-game-mouse-pos []
  (pintoid.client.layer/to-game-coords
   (point->vec (.. pintoid.client.graphics/pixi-renderer
                   -plugins -interaction -mouse -global))))

(defn get-user-input-state []
  (let [[mx my] (get-game-mouse-pos)
        [px py] (:position (player-entity @world))
        a (js/Math.atan2 (- my py) (- mx px))
        ac @active-keys
        tc1 #(if (ac %1) %2 0)]
    {:engine-dir (+ (tc1 :space 1) (tc1 :shift -1))
     :rotate a
     :fire?     (ac :mouse-left)
     :alt-fire? (ac :mouse-right)}))
