(ns pintoid.client.graphics.sprite
  (:require
   [cljsjs.pixi]
   [pintoid.client.graphics.animation :as a]
   [pintoid.client.graphics.animloop :as al]
   [taoensso.timbre :as timbre])
  (:require-macros
   [pintoid.client.macros :refer [goog-base goog-extend]]))

;; name -> graphical object info (prototype)
(def empty-sprite-proto {:type :sprite :texture "/img/clojure.png" :anchor [0.5 0.5]})
(def prototypes (atom {}))

;; texture id -> texture object
(def textures (atom {}))


(defn add-prototype [id proto]
  ;; Invalidate/recreate/mark all affected sprites.
  (swap! prototypes assoc id proto))


(defn- create-texture-object [{:keys [id image]}]
  (js/PIXI.Texture.fromImage image))


(defn add-texture [id tinfo]
  ;; TODO: Invalidate/recreate/mark all affected sprites.
  (let [t (create-texture-object tinfo)]
    (js/PIXI.Texture.addTextureToCache (name id) t)
    (swap! textures assoc id t)))


(defn get-texture [t]
  (if (string? t)
    (js/PIXI.Texture.fromImage t)
    (get @textures t)))


;; TODO: Add entity as optional 2nd argument.
(defmulti construct-sprite-object :type)


(defn make-sprite [spec]
  (construct-sprite-object
   (if (map? spec)
     spec
     (get @prototypes spec))))


(defmethod construct-sprite-object :default [proto]
  (construct-sprite-object empty-sprite-proto))


(defn- to-point [xy]
  (if (sequential? xy)
    (let [[x y] xy] (js/PIXI.Point. x y))
    (let [a (float xy)] (js/PIXI.Point. a a))))


(defn set-sprite-properties [obj props]
  (when-let [position (get props :position)] (set! (.-position obj) (to-point position)))
  (when-let [scale (get props :scale)] (set! (.-scale obj) (to-point scale)))
  (when-let [pivot (get props :pivot)] (set! (.-pivot obj) (to-point pivot)))
  (when-let [rotation (get props :rotation)] (set! (.-rotation obj) rotation))
  (when-let [alpha (get props :alpha)] (set! (.-alpha obj) alpha))
  (when-let [visible (get props :visible)] (set! (.-visible obj) visible))
  (when-let [anchor (get props :anchor)] (set! (.-anchor obj) (to-point anchor))))


(defn- construct-and-add-children [obj ch-protos]
  (doseq [cp ch-protos]
    (let [c (make-sprite cp)]
      (.addChild obj c))))


(defmethod construct-sprite-object :sprite [proto entity]
  (let [t (get-texture (get proto :texture ::clojure))
        s (js/PIXI.Sprite. t)]
    (construct-and-add-children s (:children proto))
    (set-sprite-properties s proto)
    s))


(defmethod construct-sprite-object :container [proto]
  (let [s (js/PIXI.Container.)]
    (construct-and-add-children s (:children proto))
    (set-sprite-properties s proto)
    s))


(def animation-counter (atom 0))

;; TODO: Extend PIXI.DisplayObject instead of Container.
(goog-extend Animator js/PIXI.Container
  ([child animator]
   (this-as this
     (goog-base this)
     (set! (.-child this) child)
     (set! (.-animationid this) (str "ac" (swap! animation-counter inc)))
     (set! (.-animator this) animator)
     (.addChild this child)
     this))
  (play []
    (this-as this
      (al/animate! (.-animationid this)
                   #((.-animator this) (.-child this) %))))
  (stop []
    (this-as this
      (al/animate! (.-animationid this)
                   nil)))
  (destroy [& args]
    (this-as this
      (.stop this))))


(def ^:const pi js/Math.PI)
(def ^:const pi2 (* 2 pi))

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


(defmethod construct-sprite-object :animator [proto]
  (let [a (make-animator-fn proto)
        c (make-sprite (:child proto))
        s (Animator. c a)]
    (set-sprite-properties s proto)
    (.play s)
    s))
