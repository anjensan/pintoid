(ns pintoid.client.graphics.sprite
  (:require
   [cljsjs.pixi]
   [pintoid.client.asset :as as]
   [pintoid.client.graphics.utils :refer [pi pi2 to-point]]
   [pintoid.client.graphics.animation :as a]
   [pintoid.client.graphics.animloop :as al]
   [taoensso.timbre :as timbre :include-macros true])
  (:require-macros
   [pintoid.client.macros :refer [defjsclass call-super]]))

(def empty-sprite-proto
  {:class :sprite
   :type :sprite
   :texture "/img/clojure.png"
   :anchor [0.5 0.5]})


(def textures (atom))
(def sprite-protos (atom))
(def animation-counter (atom 0))


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


(defmethod as/load-asset :sprite [id sprite]
  (swap! sprite-protos assoc id sprite))


(defmethod as/unload-asset :sprite [id sprite]
  (swap! dissoc sprite-protos id))


(defmethod as/get-asset :sprite [class id]
  (get @sprite-protos id))


(defmulti construct-sprite-object
  (fn [proto props] (:type proto)))


(defn get-sprite-spec [id]
  (cond
    (map? id) id
    (keyword? id) (get @sprite-protos id)))


(defn set-sprite-properties! [obj props]
  (when-let [position (get props :position)] (set! (.-position obj) (to-point position)))
  (when-let [scale (get props :scale)] (set! (.-scale obj) (to-point scale)))
  (when-let [pivot (get props :pivot)] (set! (.-pivot obj) (to-point pivot)))
  (when-let [rotation (get props :rotation)] (set! (.-rotation obj) rotation))
  (when-let [alpha (get props :alpha)] (set! (.-alpha obj) alpha))
  (when-let [visible (get props :visible)] (set! (.-visible obj) visible))
  (when-let [anchor (get props :anchor)] (set! (.-anchor obj) (to-point anchor))))



(defmethod construct-sprite-object :default [proto props]
  (construct-sprite-object empty-sprite-proto nil))


(defn set-tiling-sprite-properties! [obj props]
  (when-let [tile-position (get props :tile-position)]
    (set! (.-tilePosition obj) (to-point tile-position)))
  (when-let [tile-scale (get props :tile-scale)]
    (set! (.-scale obj) (to-point tile-scale))))


(defn make-sprite
  ([spec]
   (make-sprite spec nil))
  ([spec props]
   (construct-sprite-object spec props)))


(defmethod construct-sprite-object :default [proto props]
  (construct-sprite-object empty-sprite-proto props))


(defn- construct-and-add-children [obj ch-protos]
  (doseq [cp ch-protos]
    (let [c (make-sprite cp)]
      (.addChild obj c))))


(defmethod construct-sprite-object :sprite [proto props]
  (let [t (get-texture (get proto :texture ::clojure))
        s (js/PIXI.Sprite. t)]
    (construct-and-add-children s (:children proto))
    (set-sprite-properties! s proto)
    (set-sprite-properties! s props)
    s))


(defmethod construct-sprite-object :container [proto props]
  (let [s (js/PIXI.Container.)]
    (construct-and-add-children s (:children proto))
    (set-sprite-properties! s proto)
    s))


(defmethod construct-sprite-object :tiling-sprite [proto props]
  (let [t (get-texture (get proto :texture ::clojure))
        h (:height proto)
        w (:width proto)
        s (js/PIXI.extras.TilingSprite. t w h)]
    (construct-and-add-children s (:children proto))
    (set-sprite-properties! s proto)
    (set-sprite-properties! s props)
    (set-tiling-sprite-properties! s proto)
    (set-tiling-sprite-properties! s props)
    s))


;; TODO: Extend PIXI.DisplayObject instead of Container.
(defjsclass Animator js/PIXI.Container
  (constructor [this child animator]
   (call-super Animator this .constructor)
   (set! (.-child this) child)
   (set! (.-animationid this) (str "ac" (swap! animation-counter inc)))
   (set! (.-animator this) animator)
   (.addChild this child))
  (play [this]
   (al/animate! (.-animationid this) #((.-animator this) (.-child this) %)))
  (stop [this]
   (al/animate! (.-animationid this) nil))
  (destroy [this]
   (.stop this)
   (call-super Animator this .destroy)))


(defn wave-function [{:keys [kind period shift min max power]
                      :or {kind :saw period 1000 shift 0 min 0 max 1 power 1}}]
  (let [shift (if (= :random shift) (rand-int period) shift)
        normx #(-> % (+ shift) (/ period) (mod 1))
        powx (if (== 1 power)
               identity
               #(* (js/Math.sign %)
                   (js/Math.pow (js/Math.abs %) power)))
        dx (- max min)
        mmx #(+ min (* dx %))
        funx (case kind
               :saw #(-> %)
               :sin #(-> % (* pi2) js/Math.sin (+ 1) (* 0.5))
               :cos #(-> % (* pi2) js/Math.cos (+ 1) (* 0.5))
               :sqr #(if (< (mod % 1) 0.5) 0 1)
               :tri #(if (< % 0.5)
                       (-> % (* 2))
                       (->> % (* 2) (- 2))))]
    #(-> % normx funx powx mmx)))


(defn make-animator-fn [proto]
  {:pre [(not (and (:a-scale proto)
                   (or (:a-scale-x proto)
                       (:a-scale-y proto))))]}
  (let [shift (case (:shift proto :random)
                :random (rand-int 1e10)
                :start (- (int (js/performance.now)))
                :none 0
                (-> proto :shift int))
        wwf (fn [k setter]
              (when-let [fs (get proto k)]
                (let [wf (wave-function fs)]
                  (fn [obj t] (setter obj (wf t))))))
        maybe-anims
        [(wwf :a-rotation #(set! (.-rotation %1) %2))
         (wwf :a-alpha #(set! (.-aplha %1) %2))
         (wwf :a-position-x #(set! (.. %1 -position -x) %2))
         (wwf :a-position-y #(set! (.. %1 -position -y) %2))
         (wwf :a-scale #(set! (.-scale %1) (js/PIXI.Point. %2 %2)))
         (wwf :a-scale-x #(set! (.. %1 -scale -x) %2))
         (wwf :a-scale-y #(set! (.. %1 -scale -y) %2))]
        anims (vec (remove nil? maybe-anims))]
    (if (== 1 (count anims))
      (let [f (first anims)]
        #(f %1 (+ %2 shift)))
      (fn [obj t]
        (let [tt (+ t shift)]
          (run! #(% obj tt) anims))))))


(defmethod construct-sprite-object :animator [proto props]
  (let [a (make-animator-fn proto)
        c (make-sprite (:child proto))
        s (Animator. c a)]
    (set-sprite-properties! s proto)
    (set-sprite-properties! s props)
    (.play s)
    s))
