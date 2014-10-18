(ns pintoid.client.graphics
  (:use [pintoid.client.animation :only
         [linear-move!
          last-animation-time
          ]]))

;; ---

(def bg-texture :white)
(def textures [:white :clojure])

(def camera-movement-duration 800)

(def map-width 10000)
(def map-height 10000)
(def cnvs-width (- (.-innerWidth js/window) 100))
(def cnvs-height (- (.-innerHeight js/window) 100))

(def pixi-stage (new js/PIXI.Stage))
(def pixi-renderer (.autoDetectRenderer js/PIXI cnvs-width cnvs-height))
(def pixi-gamefield (new js/PIXI.DisplayObjectContainer))

(def pixi-textures (atom {}))


(declare texture-url)
(declare get-texture)
(declare add-texture)
(declare load-texture)

;; --

(defn load-textures []
  (reset! pixi-textures
   (into {}
    (map (fn [x] [x (load-texture x)]) textures))))

(defn init-pixi-renderer []
  (let [texture (get-texture bg-texture)
        ;; FIXME: don't use 100500 :)
        bg-sprite (new js/PIXI.TilingSprite texture map-width map-height)]
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

(defn add-texture [n]
  (swap! pixi-textures assoc (load-texture n)))

(defn get-texture [n]
  (get @pixi-textures n (add-texture n)))


(defn create-sprite [texture-name [x y]]
  (let [t (get-texture texture-name)
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

(defmethod create-entity-pixi-object :star [entity]
  :...)


;; ---

(defn delete-entity-pixi-object [pixi-object]
  (.removeChild pixi-gamefield pixi-object))


(defn render-graphics! []
  (.render pixi-renderer pixi-stage))
