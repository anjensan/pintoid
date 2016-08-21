(ns pintoid.client.graphics.core
  (:require
   [cljsjs.pixi]
   [pintoid.client.graphics.animation :as a]
   [pintoid.client.graphics.layer :as gl]
   [pintoid.client.graphics.sprite :as s]
   [pintoid.client.graphics.tilemap]))


;; entity id -> sprite object (DisplayObject)
(def sprites (atom {}))
(def pixi-stage nil)
(def pixi-renderer nil)



(defn- scale-canvas-to-window [c]
  (let [w js/window
        wiw (.-innerWidth w)
        wih (.-innerHeight w)
        cow (.-offsetWidth c)
        coh (.-offsetHeight c)
        sx (/ wiw cow)
        sy (/ wih coh)
        smm (/ (min sx sy) (max sx sy))
        ;; when sx ~ sy use max to avoid white borders around the canvas
        s (if (> smm 0.997) (max sx sy) (min sx sy))]
    (set! (.. c -style -transformOrigin) "0 0")
    (set! (.. c -style -transform) (str "scale(" s ")"))
    (set! (.. c -style -paddingLeft) 0)
    (set! (.. c -style -paddingRight) 0)
    (set! (.. c -style -paddingTop) 0)
    (set! (.. c -style -paddingBottom) 0)
    (set! (.. c -display) "block")
    (let [margin (str (/ (- wiw (* s cow)) 2) "px")]
      (set! (.. c -style -marginLeft) margin)
      (set! (.. c -style -marginRight) margin))
    (let [margin (str (/ (- wih (* s coh)) 2) "px")]
      (set! (.. c -style -marginTop) margin)
      (set! (.. c -style -marginBottom) margin))))


(defn init-autoscaling [canvas]
  (.addEventListener
   js/window "resize"
   (fn [event]
     (when pixi-renderer
       (scale-canvas-to-window canvas))))
  (scale-canvas-to-window canvas))


(defn init-pixi-renderer [width height]
  (set! pixi-renderer (js/PIXI.autoDetectRenderer width height))
  (set! pixi-stage (gl/init-layers-container width height))
  (.-view pixi-renderer))


(defn get-sprite [eid]
  (get @sprites eid))


(defn remove-sprite [eid]
  (when-let [obj (get @sprites eid)]
    (when-let [p (.-parent obj)]
      (.removeChild p obj))
    (swap! sprites dissoc eid)
    (.destroy obj)))


(defn new-sprite [entity]
  (when-let [eid (:eid entity)]
    (when-let [old-obj (get @sprites eid)]
      (.removeChild (.-parent old-obj) old-obj))
    (let [sprite-spec (s/get-sprite-spec (:sprite entity))
          layer-id (or (:layer entity) (:layer sprite-spec))
          sprite (s/make-sprite (:sprite entity) entity)]
      (gl/layer-add layer-id sprite)
      (swap! sprites assoc eid sprite)
      sprite)))


(defn render-graphics! []
  (.render pixi-renderer pixi-stage))
