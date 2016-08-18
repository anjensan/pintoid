(ns pintoid.client.graphics.sprite
  (:require
   [cljsjs.pixi]
   [pintoid.client.asset :as as]
   [pintoid.client.graphics.utils :refer
    [pi pi2 to-point point->vec vec->point minmax]]
   [pintoid.client.graphics.animation :as a]
   [pintoid.client.graphics.tilemap :as tm]
   [pintoid.client.graphics.animloop :as al]
   [taoensso.timbre :as timbre :include-macros true])
  (:require-macros
   [pintoid.client.macros :refer [defjsclass call-super foreach!]]))


(def empty-sprite-proto
  {:class :sprite
   :type :sprite
   :texture "/img/clojure.png"
   :anchor [0.5 0.5]})

(def textures (atom))
(def sprites (atom))


;; == Textures

(defn- create-texture-object [{:keys [id image]}]
  (js/PIXI.Texture.fromImage image))


(defmethod as/load-asset :texture [id tinfo]
  ;; TODO: Invalidate/recreate/mark all affected sprites.
  (let [t (create-texture-object tinfo)]
    (js/PIXI.Texture.addTextureToCache (name id) t)
    (swap! textures assoc id t)))


(defmethod as/unload-asset :texture [id texture]
  (js/PIXI.Texture.removeTextureToCache (name id))
  (swap! textures dissoc id))


(defn- get-texture [t]
  (if (string? t)
    (js/PIXI.Texture.fromImage t)
    (get @textures t)))


(defmethod as/get-asset :texture [class id]
  (get-texture id))


;; == Sprites - common

(defmulti create-sprite-factory
  (fn [proto] (:type proto)))


(defmethod as/load-asset :sprite [id sprite]
  (swap! sprites assoc id
         {:proto sprite :factory (create-sprite-factory sprite)}))


(defmethod as/unload-asset :sprite [id sprite]
  (swap! dissoc sprites id))


(defmethod as/get-asset :sprite [class id]
  (get-in @sprites [id :proto]))


(defn get-sprite-factory [id]
  (cond
    (map? id) (create-sprite-factory id)
    (keyword? id) (get-in @sprites [id :factory])))


(defn get-sprite-spec [id]
  (cond
    (map? id) id
    (keyword? id) (get-in @sprites [id :proto])))


(defn make-sprite
  ([spec]
   (make-sprite spec nil))
  ([spec props]
   ((get-sprite-factory spec) props)))


(defn set-sprite-properties! [obj props]
  (when props
    (when-let [position (get props :position)] (set! (.-position obj) (to-point position)))
    (when-let [scale (get props :scale)] (set! (.-scale obj) (to-point scale)))
    (when-let [pivot (get props :pivot)] (set! (.-pivot obj) (to-point pivot)))
    (when-let [rotation (get props :rotation)] (set! (.-rotation obj) rotation))
    (when-let [alpha (get props :alpha)] (set! (.-alpha obj) alpha))
    (when-let [visible (get props :visible)] (set! (.-visible obj) visible))
    (when-let [anchor (get props :anchor)] (set! (.-anchor obj) (to-point anchor))))
  obj)


(defn set-tiling-sprite-properties! [obj props]
  (when props
    (when-let [tile-position (get props :tile-position)] (set! (.-tilePosition obj) (to-point tile-position)))
    (when-let [tile-scale (get props :tile-scale)] (set! (.-tileScale obj) (to-point tile-scale))))
  obj)


(defmethod create-sprite-factory :default [proto]
  (timbre/warnf "Unknown sprite: %s" proto)
  (create-sprite-factory empty-sprite-proto))


(defmethod create-sprite-factory :sprite [proto]
  (let [t (get-texture (get proto :texture ::clojure))
        child-factories (mapv get-sprite-factory (:children proto))]
    (fn [props]
      (let [s (js/PIXI.Sprite. t)]
        (foreach! [sf child-factories] (.addChild s (sf)))
        (set-sprite-properties! s proto)
        (set-sprite-properties! s props)))))


(defmethod create-sprite-factory :container [proto]
  (let [child-factories (mapv get-sprite-factory (:children proto))]
    (fn [props]
      (let [s (js/PIXI.Container.)]
        (foreach! [sf child-factories] (.addChild s (sf)))
        (set-sprite-properties! s proto)
        s))))


(defmethod create-sprite-factory :tiling-sprite [proto]
  (let [t (get-texture (get proto :texture ::clojure))
        h (:height proto)
        w (:width proto)]
    (fn [props]
      (-> (js/PIXI.extras.TilingSprite. t w h)
        (set-tiling-sprite-properties! proto)
        (set-tiling-sprite-properties! props)
        (set-sprite-properties! proto)
        (set-sprite-properties! props)))))


(defmethod create-sprite-factory :random-tilemap [proto]
  (let [tiles-factories (mapv get-sprite-factory (:tiles proto))
        create-sprite #((nth tiles-factories (mod (hash %) (count tiles-factories))))
        tmfactory (tm/make-tilemap-sprite-factory create-sprite proto)]
    (fn [props]
      (-> (tmfactory props)
          (set-sprite-properties! proto)
          (set-sprite-properties! props)))))


(defmethod create-sprite-factory :animator [proto]
  (let [child-factory (get-sprite-factory (:child proto))
        afactory (a/make-sprite-animator-factory child-factory proto)]
    (fn [props]
      (-> (afactory props)
          (set-sprite-properties! proto)
          (set-sprite-properties! props)))))
