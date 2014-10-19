(ns pintoid.client.graphics
  (:use [pintoid.client.animation :only
         [linear-move!
          last-animation-time
          ]])
  (:require-macros
   [pintoid.client.utils :refer [log]]
   ))

;; ---

(def bg-texture :back)
(def textures [:white :clojure :black :racket_blue :racket_red
               :star1])

(def camera-movement-duration 800)

(def map-width 5000)
(def map-height 5000)
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
        bg-sprite (new js/PIXI.TilingSprite texture map-width map-height)
        bg-sprite-pos (.-position bg-sprite)]
    (.addChild pixi-stage pixi-gamefield)
    (set! (.-x bg-sprite-pos) (- (/ map-width 2)))
    (set! (.-y bg-sprite-pos) (- (/ map-height 2)))
    (.addChild pixi-gamefield bg-sprite))
   (.-view pixi-renderer))


(defn- current-camera-position []
  (let [p (.-position pixi-gamefield)]
    [(.-x p) (.-y p)]))


(defn move-player-camera! [t1 t2 xy1 xy2]
  (let [neg (fn [[x y]] [(- (/ cnvs-width 2) x)
                         (- (/ cnvs-height 2) y)])]
    (linear-move!
     nil
     pixi-gamefield
     t1
     t2
     (neg xy1)
     (neg xy2)
     )))

;; ---

(defn texture-url [texture]
  (str "/img/" (name texture) ".png"))

(defn load-texture [n]
  (.fromImage js/PIXI.Texture (texture-url n)))

(defn add-texture [n]
  (let [t (load-texture n)]
    (swap! pixi-textures assoc t)
    t))

(defn get-texture [n]
  (or (get @pixi-textures n) (add-texture n)))

(defn create-sprite
  ([texture-name [x y] [ax ay]]
     (let [t (get-texture texture-name)
           s (js/PIXI.Sprite. t)
           p (.-position s)
           a (.-anchor s)]
       (set! (.-x p) x)
       (set! (.-y p) y)
       (set! (.-x a) ax)
       (set! (.-y a) ay)
       s))
  ([texture-name xy]
     (create-sprite texture-name xy [0.5 0.5])))


(defn add-to-gamefield! [pobj pixi-filter]
  (when pixi-filter (set! (.-filters pobj) (array pixi-filter)))
  (.addChild pixi-gamefield pobj)
  pobj)


(defmulti create-entity-pixi-object (comp keyword :type))

(defmethod create-entity-pixi-object :default [entity]
  (let [texture (:texture entity :clojure)
        xy (:xy entity [0 0])
        sprite (create-sprite texture xy)]
    (add-to-gamefield! sprite nil)))


(defmethod create-entity-pixi-object :player [entity]
  (add-to-gamefield!
   (create-sprite :racket_blue (:xy entity)) nil))


(defmethod create-entity-pixi-object :black [entity]
  (let [pixi-filter (js/PIXI.TwistFilter.)
        filter-offcet (.-offset pixi-filter)]
    (set! (.-angle pixi-filter) 10)
    (set! (.-radius pixi-filter) 0.2)
    (set! (.-x filter-offcet) 0.15)
    (set! (.-y filter-offcet) 0.27)
  (add-to-gamefield!
   (create-sprite (:texture entity :black1) (:xy entity))
   pixi-filter)))


(defmethod create-entity-pixi-object :self-player [entity]
  (add-to-gamefield!
   (create-sprite :racket_red (:xy entity)) nil))


(defmethod create-entity-pixi-object :clojure [entity]
  (add-to-gamefield!
   (create-sprite :clojure (:xy entity)) nil))


;; ---

(defn delete-entity-pixi-object [pixi-object]
  (when pixi-object
    (log :debug "delte pixi obj" pixi-object)
    (.removeChild pixi-gamefield pixi-object)))


(defn render-graphics! []
  (.render pixi-renderer pixi-stage))
