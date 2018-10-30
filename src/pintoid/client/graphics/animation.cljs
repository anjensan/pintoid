(ns pintoid.client.graphics.animation
  (:require
   [cljsjs.pixi]
   [pintoid.client.graphics.animloop :refer [animate!]]
   [pintoid.client.graphics.utils :refer [pi2]])
  (:require-macros
   [taoensso.timbre :as timbre]
   [pintoid.client.macros :refer [defjsclass call-super]]))


(def animation-uid-counter (atom 0))

(defn- object-animation-id [obj anim-kind]
  (let [aid (or
             (.--pintoid-anim-uid obj)
             (set! (.--pintoid-anim-uid obj)
                   (swap! animation-uid-counter inc)))]
    (str anim-kind aid)))

(defn- mk-linear-interpolator [t1 t2 v1 v2]
  (let [t2-t1 (- t2 t1)
        ddc (/ 1 t2-t1)]
    (fn [t]
      (let [dd (* (- t2 t) ddc)
            sd (- 1 dd)]
        (+ (* v1 dd) (* v2 sd))))))


(defn- anim-linear-updater
  [obj t1 t2 xy1 xy2]
  (let [p (.-position obj)
        [x1 y1] xy1
        [x2 y2] xy2
        xi (mk-linear-interpolator t1 t2 x1 x2)
        yi (mk-linear-interpolator t1 t2 y1 y2)]
    (fn [time]
      (let [x' (xi time)
            y' (yi time)]
        (set! (.-x p) x')
        (set! (.-y p) y')))))


(defn- anim-linear-finisher
  [obj xy2]
  (fn []
    (when-let [p (.-position obj)]
      (let [[x2 y2] xy2]
        (set! (.-x p) x2)
        (set! (.-y p) y2)))))


(defn- anim-linear-rotate-updater [obj t1 t2 angle1 angle2]
  (let [ai (mk-linear-interpolator t1 t2 angle1 angle2)]
    (fn [t]
      (set! (.-rotation obj) (ai t)))))


(defn- anim-linear-rotate-finisher [obj angle]
  (fn []
    (set! (.-rotation obj) angle)))


(defn linear-move [obj t1 t2 xy1 xy2]
  (timbre/trace "linear-move" obj t1 t2 xy1 xy2)
  (animate!
   (object-animation-id obj "lm") t1 t2
   (when xy1 (anim-linear-updater obj t1 t2 xy1 xy2))
   nil
   (anim-linear-finisher obj xy2)))


(defn instant-move [obj t1 t2 xy]
  (timbre/trace "instant-move" obj t1 t2 xy)
  (animate!
   (object-animation-id obj "lm") t1 t2
   nil
   nil
   (anim-linear-finisher obj xy)))


(defn linear-rotate [obj t1 t2 angle1 angle2]
  (timbre/trace "linear-rotate" obj t1 t2 angle1 angle2)
  (animate!
   (object-animation-id obj "rot") t1 t2
   (when angle1 (anim-linear-rotate-updater obj t1 t2 angle1 angle2))
   nil
   (anim-linear-rotate-finisher obj angle2)
   ))


(defn instant-rotate [obj t1 t2 angle]
  (timbre/trace "instant-rotate" obj t1 t2 angle)
  (animate!
   (object-animation-id obj "rot") t1 t2
   nil
   nil
   (anim-linear-rotate-finisher obj angle)
   ))


(defn linear-animate [aid t1 t2 v1 v2 setter]
  (let [ai (mk-linear-interpolator t1 t2 v1 v2)]
    (animate!
     aid t1 t2
     (fn [t] (setter (ai t)))
     nil
     (fn [] (setter v2)))))


;; Animator wrapper

(defjsclass AnimatorImpl js/PIXI.Container
  (constructor [this child animate-fn]
   (call-super AnimatorImpl this .constructor)
   (set! (.-animationid this) (object-animation-id child "a8r"))
   (set! (.-doanimate this) (fn [t] (animate-fn child t)))
   (.addChild this child)
   (.play this))
  (play [this]
   (animate! (.-animationid this) (.-doanimate this)))
  (stop [this]
   (animate! (.-animationid this) nil))

  (destroy [this]
           (.stop this)
           (set! (.-doanimate this) nil)
           (call-super AnimatorImpl this .destroy)))


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
               :sin #(-> % (* 6) js/Math.sin (+ 1) (* 0.5))
               :cos #(-> % (* 6) js/Math.cos (+ 1) (* 0.5))
               :sqr #(if (< (mod % 1) 0.5) 0 1)
               :tri #(if (< % 0.5)
                       (-> % (* 2))
                       (->> % (* 2) (- 2))))]
    #(-> % normx funx powx mmx)))


(defn make-animation-function [proto]
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
         (wwf :a-position-x #(when-let [p (.-position %1)] (set! (.-x p) %2)))
         (wwf :a-position-y #(when-let [p (.-position %1)] (set! (.-y p) %2)))
         (wwf :a-scale #(set! (.-scale %1) (js/PIXI.Point. %2 %2)))
         (wwf :a-scale-x #(when-let [s (.-scale %1)] (set! (.-x s) %2)))
         (wwf :a-scale-y #(when-let [s (.-scale %1)] (set! (.-y s) %2)))]
        anims (vec (remove nil? maybe-anims))]
    (if (== 1 (count anims))
      (let [f (first anims)]
        #(f %1 (+ %2 shift)))
      (fn [obj t]
        (let [tt (+ t shift)]
          (run! #(% obj tt) anims))))))


(defn make-sprite-animator-factory [create-sprite proto]
  (let [animfn (make-animation-function proto)]
    (fn [props]
      (AnimatorImpl. (create-sprite props) animfn))))
