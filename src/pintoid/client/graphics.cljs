(ns pintoid.client.graphics
  (:use [pintoid.client.animation]))

;; ---

(def bg-texture-url "...")
(def default-strite-url "...")

(def pixi-stage (new js/PIXI.Stage))
(def pixi-gamefield (new js/PIXI.DisplayObjectContainer))


(defn init-pixi-canvas! []
  (let [bg-texture (.fromImage js/PIXI.Texture bg-texture-url)
        ;; FIXME: don't use 100500 :)
        bg-sprite (new js/PIXI.TilingSprite bg-texture 100500 100500)]
    (.addChild pixi-stage pixi-gamefield)
    (.addChild pixi-gamefield bg-sprite)))


(defn- current-camera-position []
  (let [p (.-position pixi-gamefield)]
    [(.-x p) (.-y p)]))


(defn move-player-camera! [xy]
  (linear-move!
   :move-camera
   pixi-gamefield
   current-time
   (+ current-time camera-movement-duration)
   (current-camera-position)
   xy
   ))

;; ---

(defmulti create-entity-pixi-object :type)

(defmethod create-entity-pixi-object :default [entity]
  (let [img-uri (:sprite-url entity default-strite-url)
        texture (.fromImage js/PIXI.Texture img-uri)
        pobj (js/PIXI.Sprite. texture)
        position (.-position pobj)
        ;; FIXME
        [x y] (:xy entity [0 0])
        [w h] (:wh entity [100 100])
        ]
    (set! (.-width pobj) w)
    (set! (.-height pobj) h)
    (set! (.-x position) x)
    (set! (.-y position) y)
    (.addChild pixi-gamefield pobj)
    pobj))


(defmethod create-entity-pixi-object :bot [entity]
  :...)


(defmethod create-entity-pixi-object :asteroid [entity]
  :...)


;; ---

(defn delete-entity-pixi-object [entity]
  :...)
