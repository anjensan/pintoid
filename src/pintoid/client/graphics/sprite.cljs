(ns pintoid.client.graphics.sprite
  (:require
   [cljsjs.pixi]
   [pintoid.client.asset :as as]
   [pintoid.client.graphics.utils :refer
    [pi pi2 ->rectangle ->point point->vec
     ->blendmode vec->point minmax]]
   [pintoid.client.graphics.animation :as a]
   [pintoid.client.graphics.tilemap :as tm]
   [pintoid.client.graphics.animloop :as al]
   [taoensso.timbre :as timbre :include-macros true])
  (:require-macros
   [pintoid.client.macros :refer [defjsclass call-super foreach!]]))


(def empty-texture (js/PIXI.Texture.fromImage "/img/clojure.png"))
(declare empty-sprite-factory)
(def empty-sprite-proto
  {:class :sprite
   :type :sprite
   :texture "/img/clojure.png"
   :anchor [0.5 0.5]})

(def textures (atom))
(def sprites (atom))


;; == Textures

(declare get-texture)

(defn- create-texture-object
  [{:keys [id image base frame
           crop trim rotate]}]
  (if image
    (do
      (assert (every? nil? [base frame crop trim rotate]))
      (js/PIXI.Texture.fromImage image))
    (do
      (assert (nil? image))
      (js/PIXI.Texture.
       (get-texture base)
       (->rectangle frame)
       (->rectangle crop)
       (->rectangle trim)
       rotate))))


(defmethod as/load-asset :texture [id tinfo]
  ;; TODO: Invalidate/recreate/mark all affected sprites.
  (let [t (create-texture-object tinfo)]
    (js/PIXI.Texture.addTextureToCache (name id) t)
    (swap! textures assoc id t)))


(defmethod as/unload-asset :texture [id texture]
  (js/PIXI.Texture.removeTextureToCache (name id))
  (swap! textures dissoc id))


(defn- get-texture [t]
  (cond
    (string? t) (js/PIXI.Texture.fromImage t)
    (and (keyword? t) (contains? @textures t)) (@textures t)
    :else (do (timbre/warnf "Unknown texture %s" t)
              empty-texture)))


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
    (and (keyword? id) (contains? @sprites id)) (get-in @sprites [id :factory])
    :else
    (do
      (timbre/warnf "Unknown sprite %s" id)
      empty-sprite-factory)))


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
    (when-let [position (get props :position)] (set! (.-position obj) (->point position)))
    (when-let [scale (get props :scale)] (set! (.-scale obj) (->point scale)))
    (when-let [pivot (get props :pivot)] (set! (.-pivot obj) (->point pivot)))
    (when-let [rotation (get props :rotation)] (set! (.-rotation obj) rotation))
    (when-let [alpha (get props :alpha)] (set! (.-alpha obj) alpha))
    (when-let [visible (get props :visible)] (set! (.-visible obj) visible))
    (when-let [anchor (get props :anchor)] (set! (.-anchor obj) (->point anchor)))
    (when-let [bm (get props :blend-mode)]
      ;; FIXME: Temp workaround - 'blendMode' is not externed in cljsjs.pixi.
      (aset obj "blendMode" (->blendmode bm)))
    )
  obj)


(defn set-tiling-sprite-properties! [obj props]
  (when props
    (when-let [tile-position (get props :tile-position)]
      (set! (.-tilePosition obj) (->point tile-position)))
    (when-let [tile-scale (get props :tile-scale)]
      (set! (.-tileScale obj) (->point tile-scale))))
  obj)


(defn set-text-properties! [obj props]
  (when props
    (when-let [text (get props :text)]
      (set! (.-text obj) text))
    (when-let [style (get props :style)]
      (set! (.-style obj) (clj->js style))))
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
        hs (get proto :hash-seed "")
        create-sprite (fn [[c r]]
                        ((nth tiles-factories
                              (mod (hash (str hs "-" c "-" r)) (count tiles-factories)))))
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


(defmethod create-sprite-factory :text [proto]
  (fn [props]
    (-> (js/PIXI.Text. (:text props ""))
        (set-sprite-properties! proto)
        (set-text-properties! proto)
        (set-sprite-properties! props)
        (set-text-properties! props))))
