(ns pintoid.client.graphics.core
  (:require [pintoid.client.graphics.animation :as a]
            [pintoid.client.graphics.sprite :as s])
  (:require-macros [pintoid.client.macros :refer [log]]))


;; entity id -> sprite object (DisplayObject)
(def sprites (atom {}))

;; Root sprite.
(def pixi-stage (js/PIXI.Stage.))
(def pixi-gamefield (js/PIXI.DisplayObjectContainer.))
(def pixi-renderer nil)

(def ^:dynamic *root* pixi-gamefield)

(def map-width 5000)
(def map-height 5000)
(def canvas-width 1600)
(def canvas-height 900)

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
        s (if (> smm 0.996) (max sx sy) (min sx sy))]
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

(defn init-pixi-renderer []
  (let [renderer (.autoDetectRenderer js/PIXI canvas-width canvas-height)
        texture (s/get-texture "/img/back.png")
        bg-sprite (new js/PIXI.TilingSprite texture map-width map-height)
        bg-sprite-pos (.-position bg-sprite)]
    (.addChild pixi-stage pixi-gamefield)
    (set! (.. pixi-gamefield -position -x) (/ canvas-width 2))
    (set! (.. pixi-gamefield -position -y) (/ canvas-height 2))
    (set! (.-x bg-sprite-pos) (- (/ map-width 2)))
    (set! (.-y bg-sprite-pos) (- (/ map-height 2)))
    (set! pixi-renderer renderer)
    (.addChild pixi-gamefield bg-sprite)
    (.-view pixi-renderer)))


(defn move-player-camera! [t1 t2 xy1 xy2]
  (let [[x1 y1] xy1, [x2 y2] xy2]
    (a/linear-animate "camera-x" t1 t2 x1 x2 #(set! (.. pixi-gamefield -pivot -x) %))
    (a/linear-animate "camera-y" t1 t2 y1 y2 #(set! (.. pixi-gamefield -pivot -y) %)))
  )


(defn get-sprite [eid]
  (get @sprites eid))


(defn remove-sprite [eid]
  (when-let [obj (get @sprites eid)]
    (when obj.parent
      (.removeChild obj.parent obj))
    (swap! sprites dissoc eid)
    (.destroy obj)))


(defn new-sprite [entity]
  (when-let [eid (:eid entity)]
    (when-let [old-obj (get @sprites eid)]
      (.removeChild (.-parent old-obj) old-obj))
    (let [obj (s/make-sprite (:sprite entity))]
      (s/set-sprite-properties obj entity)
      (.addChild *root* obj)
      (swap! sprites assoc eid obj)
      obj)))

(defn render-graphics! []
  (.render pixi-renderer pixi-stage))
