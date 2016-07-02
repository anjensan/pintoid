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
(def cnvs-width (- (.-innerWidth js/window) 100))
(def cnvs-height (- (.-innerHeight js/window) 100))

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

(defn update-player-score! [value]
  (let [text (str "Score: " value)]
    (set! (.-text player-score-value) text)))

(defn update-player-death! [value]
  (let [text (str "Death: " value)]
    (set! (.-text player-death-value) text)))

(defn init-pixi-renderer []
  (let [renderer (.autoDetectRenderer js/PIXI cnvs-width cnvs-height)
        texture (s/get-texture "/img/back.png")
        bg-sprite (new js/PIXI.TilingSprite texture map-width map-height)
        bg-sprite-pos (.-position bg-sprite)]
    (.addChild pixi-stage pixi-gamefield)
    (set! (.-x bg-sprite-pos) (- (/ map-width 2)))
    (set! (.-y bg-sprite-pos) (- (/ map-height 2)))
    (set! pixi-renderer renderer)
    (.addChild pixi-gamefield bg-sprite)
    (.-view pixi-renderer)))


(defn move-player-camera! [t1 t2 xy1 xy2]
  (let [neg (fn [[x y]] [(- (/ cnvs-width 2) x)
                         (- (/ cnvs-height 2) y)])]
    (a/linear-move
     pixi-gamefield
     t1
     t2
     (neg xy1)
     (neg xy2)
     )))


(defn render-graphics! []
  (.render pixi-renderer pixi-stage))


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
