(ns pintoid.client.graphics.core
  (:require
   [pintoid.client.graphics.animation :as a]
   [pintoid.client.graphics.layer :as gl]
   [pintoid.client.graphics.sprite :as s]))


;; entity id -> sprite object (DisplayObject)
(def sprites (atom {}))
(def pixi-stage (js/PIXI.Stage.))
(def pixi-renderer nil)

(def player-score-value (js/PIXI.Text. "Score: 0"))
(def player-death-value (js/PIXI.Text. "Death: 0"))

(declare texture-url)
(declare get-texture)
(declare load-all-textures)

;; --

(defn init-pixi-labels []
  (let [score-pos (.-position player-score-value)
        death-pos (.-position player-death-value)
        style (js-obj "fill" "white" "font" "normal 22px Arial")]
    ;; TODO: Move to ::layer/hud.
    (set! (.-x score-pos) 3)
    (set! (.-y score-pos) 3)
    (set! (.-x death-pos) 3)
    (set! (.-y death-pos) 27)
    (.setStyle player-score-value style)
    (.setStyle player-death-value style)
    (.addChild pixi-stage player-score-value)
    (.addChild pixi-stage player-death-value)))


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


(defn update-player-score! [value]
  (let [text (str "Score: " value)]
    (set! (.-text player-score-value) text)))

(defn update-player-death! [value]
  (let [text (str "Death: " value)]
    (set! (.-text player-death-value) text)))

(defn init-pixi-renderer [width height]
  (set! pixi-renderer (.autoDetectRenderer js/PIXI width height))
  (.addChild pixi-stage (gl/init-layers-container width height))
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
          sprite (s/make-sprite sprite-spec entity)]
      (gl/layer-add layer-id sprite)
      (swap! sprites assoc eid sprite)
      sprite)))


(defn render-graphics! []
  (.render pixi-renderer pixi-stage))
