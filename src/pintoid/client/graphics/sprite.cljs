(ns pintoid.client.graphics.sprite
  (:require [cljsjs.pixi]))

;; name -> graphical object info (prototype)
(def empty-sprite-proto {:type :sprite :texture "/img/clojure.png" :anchor [0.5 0.5]})
(def prototypes (atom {}))

;; texture id -> texture object
(def textures (atom {}))


(defn add-prototype [proto]
  ;; Invalidate/recreate/mark all affected sprites.
  (swap! prototypes assoc (:id proto) proto))


(defn- create-texture-object [{:keys [id image]}]
  (js/PIXI.Texture.fromImage image))


(defn add-texture [{:keys [id] :as tinfo}]
  ;; TODO: Invalidate/recreate/mark all affected sprites.
  (let [t (create-texture-object tinfo)]
    (js/PIXI.Texture.addTextureToCache (name id) t)
    (swap! textures id t)))


(defn get-texture [t]
  (if (string? t)
    (js/PIXI.Texture.fromImage t)
    (get @textures t)))


;; TODO: Add entity as optional 2nd argument.
(defmulti construct-sprite-object
  (fn [obj-info] (:type obj-info)))


(defn make-sprite [id]
  (construct-sprite-object (get @prototypes id empty-sprite-proto)))


(defmethod construct-sprite-object :default [proto]
  (construct-sprite-object empty-sprite-proto))


(defn- vec-to-point [[x y]]
  (js/PIXI.Point. x y))


(defn populate-display-object-properties [obj props]
  (when-let [position (get props :position)] (set! (.-position obj) (vec-to-point position)))
  (when-let [pivot (get props :pivot)] (set! (.-pivot obj) (vec-to-point pivot)))
  (when-let [rotation (get props :rotation)] (set! (.-rotation obj) rotation))
  (when-let [alpha (get props :alpha)] (set! (.-alpha obj) alpha))
  (when-let [scale (get props :scale)] (set! (.-scale obj) scale))
  (when-let [visible (get props :visible)] (set! (.-visible obj) visible)))


(defn populate-sprite-properties [obj props]
  (populate-display-object-properties obj props)
  (when-let [anchor (get props :anchor)] (set! (.-anchor obj) (vec-to-point anchor))))


(defn- construct-and-add-children [obj ch-protos]
  (doseq [cp ch-protos]
    (let [c (construct-sprite-object cp)]
      (.addChild obj c))))


(defmethod construct-sprite-object :sprite [proto]
  (let [t (get-texture (get proto :texture ::clojure))
        s (js/PIXI.Sprite. t)]
    (construct-and-add-children s (:children proto))
    (populate-sprite-properties s proto)
    s))


(defmethod construct-sprite-object :container [proto]
  (let [s (js/PIXI.Container.)]
    (populate-sprite-properties s proto)
    (construct-and-add-children s (:children proto))
    s))