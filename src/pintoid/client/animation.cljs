(ns pintoid.client.animation
  (:require
   [cljsjs.pixi]
   [pintoid.client.animloop :refer [action! animate!]]
   [pintoid.client.utils :refer [pi2]])
  (:require-macros
   [taoensso.timbre :as timbre]
   [pintoid.client.macros :refer [defjsclass call-super]]))

(defn mark-no-rotatable
  ([obj] (mark-no-rotatable obj true))
  ([obj b] (set! (.-notrotatable obj) (boolean b))))

(defn mark-no-moveable
  ([obj] (mark-no-moveable obj true))
  ([obj b] (set! (.-nomoveable obj) (boolean b))))

(defn rotatable? [obj]
  (and obj (.-transform obj) (not (.-notrotatable obj))))

(defn moveable? [obj]
  (and obj (.-transform obj) (not (.-nomoveable obj))))

(defn- mk-linear-interpolator [t1 t2 v1 v2]
  (let [t2-t1 (- t2 t1)
        ddc (/ 1 t2-t1)]
    (fn [t]
      (cond
        (== t t1) v1
        (== t t2) v2
        :else (let [dd (* (- t2 t) ddc)
                    sd (- 1 dd)]
                (+ (* v1 dd) (* v2 sd)))))))

(defn linear-move [obj t1 t2 xy1 xy2]
  (timbre/trace "linear-move" obj t1 t2 xy1 xy2)
  (when (moveable? obj)
    (animate!
     t1 t2
     (let [p (.-position obj)
           [x1 y1] xy1
           [x2 y2] xy2
           xi (mk-linear-interpolator t1 t2 x1 x2)
           yi (mk-linear-interpolator t1 t2 y1 y2)]
       (fn [t]
         (let [x' (xi t)
               y' (yi t)]
           (set! (.-x p) x')
           (set! (.-y p) y'))
         (< t t2))))))

(defn instant-move [obj t2 xy2]
  (timbre/trace "instant-move" obj t2 xy2)
  (when (moveable? obj)
    (action!
     t2
     (fn []
       (when-let [p (.-position obj)]
         (let [[x2 y2] xy2]
           (set! (.-x p) x2)
           (set! (.-y p) y2)))))))

(defn linear-rotate [obj t1 t2 angle1 angle2]
  (when (rotatable? obj)
    (timbre/trace "linear-rotate" obj t1 t2 angle1 angle2)
    (animate!
     t1 t2
     (let [ai (mk-linear-interpolator t1 t2 angle1 angle2)]
       (fn [t]
         (set! (.-rotation obj) (ai t))
         (< t t2))))))

(defn instant-rotate [obj t2 angle]
  (when (rotatable? obj)
    (timbre/trace "instant-rotate" obj t2 angle)
    (action!
     t2
     (fn []
       (set! (.-rotation obj) angle)))))

(defn linear-animate [t1 t2 v1 v2 setter]
  (animate! t1 t2 (comp setter (mk-linear-interpolator t1 t2 v1 v2))))

(defjsclass AnimatorImpl js/PIXI.Container
  (constructor [this child animate-fn]
    (call-super AnimatorImpl this .constructor)
    (set! (.-animate-fn this) (fn [t] (animate-fn child t)))
    (.addChild this child)
    (.play this))

  (play [this]
    (when-let [x (.-animate-status this)]
      (vreset! x false))
    (let [p (volatile! true)]
      (animate! (fn [t] (when @p ((.-animate-fn this) t) true)))
      (set! (.-animate-status this) p)))

  (stop [this]
    (when-let [x (.-animate-status this)]
      (vreset! x false)
      (set! (.-animate-status this) nil)))

  (destroy [this]
    (.stop this)
    (set! (.-animate-fn this) nil)
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
               :sin #(-> % (* js/Math.PI) js/Math.sin (+ 1) (* 0.5))
               :cos #(-> % (* js/Math.PI) js/Math.cos (+ 1) (* 0.5))
               :sqr #(if (< (mod % 1) 0.5) 0 1)
               :tri #(if (< % 0.5) (-> % (* 2)) (->> % (* 2) (- 2))))]
    #(-> % normx funx powx mmx)))

(defn make-animation-function [proto]
  {:pre [(not (and (:a-scale proto)
                   (or (:a-scale-x proto)
                       (:a-scale-y proto))))]}
  (let [shift (case (:shift proto :random)
                :random #(rand-int 1e10)
                :start #(- (int (js/performance.now)))
                :none (constantly 0)
                (constantly (-> proto :shift int)))
        get-shift (fn [obj]
                    (if-let [s (.-pintoid-animation-shift obj)]
                      s
                      (let [s (shift)]
                        (set! (.-pintoid-animation-shift obj) s)
                        s)))
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
        #(f %1 (+ %2 (get-shift %1))))
      (fn [obj t]
        (let [tt (+ t (get-shift obj))]
          (run! #(% obj tt) anims))))))

(defn make-sprite-animator-factory [create-sprite proto]
  (let [animfn (make-animation-function proto)]
    (fn [props]
      (AnimatorImpl. (create-sprite props) animfn))))
