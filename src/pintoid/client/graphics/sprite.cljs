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


(def empty-texture
  (js/PIXI.Texture.fromImage "/img/clojure.png"))

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
    :else (do (timbre/warnf "Unknown texture %s" t) empty-texture)))


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


(defn make-common-props-setter [props]
  (let [{:keys [position scale pivot rotation alpha visible
                anchor blend-mode mask cache-as-bitmap]} props
        mask-of (when mask (get-sprite-factory mask))
        blend-mode (->blendmode blend-mode)
        position (->point position)
        scale (->point scale)
        pivot (->point pivot)
        anchor (->point anchor)]
    (fn [obj]
      (when position (set! (.-position obj) position))
      (when scale (set! (.-scale obj) scale))
      (when pivot (set! (.-pivot obj) pivot))
      (when rotation (set! (.-rotation obj) rotation))
      (when alpha (set! (.-alpha obj) alpha))
      (when visible (set! (.-visible obj) visible))
      (when anchor (set! (.-anchor obj) anchor))
      ;; FIXME: Temp workaround - 'blendMode' is not externed in cljsjs.pixi.
      (when blend-mode (aset obj "blendMode" blend-mode))
      (when mask (set! (.-mask obj) (mask-of)))
      (when cache-as-bitmap (set! (.-cacheAsBitmap obj) cache-as-bitmap))
      obj
      )))


(defn dissoc-common-props [props]
  (dissoc
   props
   ;; TODO: Review props - inherited or not.
   :position :scale :pivot :rotation :alpha
   :visible :anchor :mask :cache-as-bitmap
   ))


(defn make-tiling-sprite-props-setter [props]
  (let [{:keys [tile-position tile-scale]} props
        tile-position (->point tile-position)
        tile-scale (->point tile-scale)]
    (fn [obj]
      (when tile-position (set! (.-tilePosition obj) tile-position))
      (when tile-scale (set! (.-tileScale obj) tile-scale))
      obj
      )))


(defn make-text-props-setter [props]
  (let [{:keys [text style]} props
        style (when style (clj->js style))]
    (fn [obj]
      (when text (set! (.-text obj) text))
      (when style (set! (.-style obj) style))
      obj
      )))


(defn- get-child-factories-seq [child]
  (cond
    (nil? child) nil
    (vector? child) (mapv get-sprite-factory child)
    (map? child) (create-sprite-factory child)
    (keyword? child) (get-sprite-factory child)))


(defmethod create-sprite-factory :default [proto]
  (timbre/warnf "Unknown sprite: %s" proto)
  (create-sprite-factory empty-sprite-proto))


(defmethod create-sprite-factory nil [proto]
  (create-sprite-factory (assoc proto :type :sprite)))


(defmethod create-sprite-factory :sprite [proto]
  (let [t (get-texture (get proto :texture :unknown))
        sps (make-common-props-setter proto)
        child-factories (get-child-factories-seq (:child proto))]
    (fn [props]
      (let [s (js/PIXI.Sprite. t)]
        (when child-factories
          (let [pp (dissoc-common-props props)]
            (foreach! [sf child-factories] (.addChild s (sf pp)))))
        (sps s)
        ((make-common-props-setter props) s)
        s))))


(defmethod create-sprite-factory :container [proto]
  (let [child-factories (get-child-factories-seq (:child proto))
        sps (make-common-props-setter proto)]
    (fn [props]
      (let [s (js/PIXI.Container.)]
        (when child-factories
          (let [pp (dissoc-common-props props)]
            (foreach! [sf child-factories] (.addChild s (sf pp)))))
        (sps s)
        ((make-common-props-setter props) s)
        s))))


(defmethod create-sprite-factory :tiling-sprite [proto]
  (let [t (get-texture (get proto :texture :unknown))
        h (:height proto)
        w (:width proto)
        sps (make-common-props-setter proto)
        tsps (make-tiling-sprite-props-setter proto)]
    (fn [props]
      (-> (js/PIXI.extras.TilingSprite. t w h)
          (tsps)
          (sps)
          ((make-tiling-sprite-props-setter props))
          ((make-common-props-setter props))
          ))))


(defmethod create-sprite-factory :random-tilemap [proto]
  (let [tiles-factories (mapv get-sprite-factory (:tiles proto))
        sps (make-common-props-setter proto)
        hs (hash (get proto :hash-seed 0))
        create-sprite (fn [[c r]]
                        ((nth tiles-factories
                              (mod (-> hs (hash-combine c) (hash-combine r))
                                   (count tiles-factories)))))
        tmfactory (tm/make-tilemap-sprite-factory create-sprite proto)]
    (fn [props]
      (-> (tmfactory props)
          (sps)
          ((make-common-props-setter props))
          ))))


(defmethod create-sprite-factory :animator [proto]
  (let [sps (make-common-props-setter proto)
        child (:child proto)
        child-factory
        (cond
          (vector? child) (create-sprite-factory {:type :container :child child})
          (map? child) (create-sprite-factory child)
          (or (keyword? child) (string? child)) (get-sprite-factory child)
          :else empty-sprite-factory)
        afactory (a/make-sprite-animator-factory child-factory proto)]
    (fn [props]
      (-> (afactory (dissoc-common-props props))
          (sps)
          ((make-common-props-setter props))
          ))))


(defmethod create-sprite-factory :text [proto]
  (let [sps (make-common-props-setter proto)
        tps (make-text-props-setter proto)]
    (fn [props]
      (-> (js/PIXI.Text. (:text props ""))
          (sps)
          (tps)
          ((make-common-props-setter props))
          ((make-text-props-setter props))
          ))))



(def empty-sprite-factory
  (create-sprite-factory empty-sprite-proto))
