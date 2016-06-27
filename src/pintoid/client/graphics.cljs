(ns pintoid.client.graphics
  (:require [pintoid.client.animation :as a])
  (:require-macros [pintoid.client.utils :refer [log]]))

;; ---

(def bg-texture :back)
(def textures [:white :clojure :black :racket_blue :racket_red :star1])

(def camera-movement-duration 800)

(def map-width 5000)
(def map-height 5000)
(def cnvs-width (- (.-innerWidth js/window) 100))
(def cnvs-height (- (.-innerHeight js/window) 100))

(def pixi-stage (new js/PIXI.Stage))
(def pixi-renderer (.autoDetectRenderer js/PIXI cnvs-width cnvs-height))

(def pixi-gamefield (new js/PIXI.DisplayObjectContainer))

(def pixi-loaded-textures (atom {}))
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
  (load-all-textures)
  (let [texture (get-texture bg-texture)
        bg-sprite (new js/PIXI.TilingSprite texture map-width map-height)
        bg-sprite-pos (.-position bg-sprite)]
    (.addChild pixi-stage pixi-gamefield)
    (set! (.-x bg-sprite-pos) (- (/ map-width 2)))
    (set! (.-y bg-sprite-pos) (- (/ map-height 2)))
    (.addChild pixi-gamefield bg-sprite))
  (init-pixi-labels)
  (update-player-score! 0)
  (update-player-death! 0)
  (.-view pixi-renderer))

(defn- current-camera-position []
  (let [p (.-position pixi-gamefield)]
    [(.-x p) (.-y p)]))

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

;; ---

(defn texture-url [texture]
  (str "/img/" (name texture) ".png"))

(defn load-texture [n]
  (swap!
   pixi-loaded-textures
   (fn [ts]
     (assoc ts n (.fromImage js/PIXI.Texture (texture-url n)))))
  (get @pixi-loaded-textures n))

(defn load-all-textures []
  (doseq [t textures]
    (load-texture t)))

(defn get-texture [n]
  (or (get @pixi-loaded-textures n)
      (load-texture n)))

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

(defn add-to-gamefield! [pobj]
  (.addChild pixi-gamefield pobj)
  pobj)


(defmulti create-entity-pixi-object (comp keyword :type))

(defmethod create-entity-pixi-object :default [entity]
  (let [texture (:texture entity :clojure)
        xy (:xy entity [0 0])]
    (add-to-gamefield! (create-sprite texture xy))))

(defmethod create-entity-pixi-object :player [entity]
  (add-to-gamefield!
   (create-sprite (if (:self-player entity) :racket_red :racket_blue) (:xy entity))))

(defmethod create-entity-pixi-object :black [entity]
  (add-to-gamefield!
   (create-sprite (:texture entity :black1) (:xy entity))))

(defmethod create-entity-pixi-object :clojure [entity]
  (add-to-gamefield!
   (create-sprite :clojure (:xy entity))))

;; ---

(defn delete-entity-pixi-object [pixi-object]
  (when pixi-object
    (log :debug "delte pixi obj" pixi-object)
    (.removeChild pixi-gamefield pixi-object)))

(defn render-graphics! []
  (.render pixi-renderer pixi-stage))
