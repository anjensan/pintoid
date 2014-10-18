(ns pintoid.client.graphics
  (:use [pintoid.client.animation :only
         [linear-move!
          last-animation-time
          ]]))

;; ---

(def bg-texture :white)
(def camera-movement-duration 800)

(def pixi-stage (new js/PIXI.Stage))
(def pixi-renderer (.autoDetectRenderer js/PIXI 1200 800))
(def pixi-gamefield (new js/PIXI.DisplayObjectContainer))


(declare texture-url)
(declare load-texture)

;; --

(defn init-pixi-renderer []
  (let [texture (load-texture bg-texture)
        ;; FIXME: don't use 100500 :)
        bg-sprite (new js/PIXI.TilingSprite texture 100500 100500)]
    (.addChild pixi-stage pixi-gamefield)
    (.addChild pixi-gamefield bg-sprite))
   (.-view pixi-renderer))


(defn- current-camera-position []
  (let [p (.-position pixi-gamefield)]
    [(.-x p) (.-y p)]))


(defn move-player-camera! [xy]
  (linear-move!
   :move-camera
   pixi-gamefield
   last-animation-time
   (+ last-animation-time camera-movement-duration)
   (current-camera-position)
   xy
   ))

;; ---

(defn texture-url [texture]
  (if (keyword? texture)
    (str "/img/" (name texture) ".png")
    (str "/img/" texture)))


(defn load-texture [n]
  (.fromImage js/PIXI.Texture (texture-url n)))


(defn create-sprite [texture-name [x y]]
  (let [t (load-texture texture-name)
        s (js/PIXI.Sprite. t)
        p (.-position s)]
    (set! (.-x p) x)
    (set! (.-y p) y)
    s))

(defn add-to-gamefield! [pobj]
  (.addChild pixi-gamefield pobj)
  pobj)

  
(defmulti create-entity-pixi-object :type)

(defmethod create-entity-pixi-object :default [entity]
  (let [texture (:texture entity :clojure)
        xy (:xy entity [0 0])
        sprite (create-sprite texture xy)]
    (add-to-gamefield! sprite)))


(defmethod create-entity-pixi-object :clojure [entity]
  (add-to-gamefield!
   (create-sprite :clojure (:xy entity))))


(defmethod create-entity-pixi-object :bot [entity]
  :...)


(defmethod create-entity-pixi-object :asteroid [entity]
  :...)


;; ---

(defn delete-entity-pixi-object [entity]
  :...)


(defn render-graphics! []
  (.render pixi-renderer pixi-stage))
