(ns pintoid.client.animation
  (:require
   [pintoid.client.utils :refer [obj-uid limit-str]]
   [goog.object])
  (:require-macros
   [pintoid.client.utils :refer [log]]))


;; -- API

(declare linear-move!)
(declare process-animation!)
(declare process-deffered-actions!)

;; -- animations

;; completely skip outdated animations
(def max-allowed-actions-lag 1000)

;; current (last) *animation* time
(def last-animation-time 0)

;; animation-id => (array t1 t2 do-anim-fn! finish-anim-fn!)
(def active-animations (js-obj))
(def deffered-actions (array))

;; anim-start-time (t1) => array of [aid [t1 t2 anim-fn! finish-anim-fn!]]
(def pending-actions-map (array))

;; sorted list of keys from #'pending-actions-map
(def pending-actions-times (array))

(defn defer-action!
  ([act-fn!] (.push deffered-actions act-fn!))
  ([act-fn! & args] (defer-action! #(apply act-fn! args))))

(defn add-action!
  [t act-fn!]
  (log :trace "add action at" t ":" (limit-str 80 act-fn!))
  (if (<= t last-animation-time)
    (act-fn!)
    (let [tx (long t)
          al (aget pending-actions-map tx)]
      (if al
        (.push al act-fn!)
        (let [al' (array)]
          (aset pending-actions-map tx al')
          (.push al' act-fn!)
          (.push pending-actions-times tx)
          (.sort pending-actions-times -))))))

(defn add-animation!
  ([aid t1 t2 animate-fn!]
     (add-animation! aid t1 t2 animate-fn! nil nil))
  ([aid t1 t2 animate-fn! init-fn! finish-fn!]
     (when (>= t2 last-animation-time)
       (let [aid (name aid)
             av (array t1 t2 animate-fn! finish-fn!)]
         (log :trace "add penging animation" aid t1 t2)
         (add-action!
          t1
          (fn []
            (when init-fn! (init-fn!))
            (aset active-animations aid av)))))))

(defn- run-scheduled-actions [time]
  (loop []
    (when-let [t (first pending-actions-times)]
      (when (<= t time)
        (when (>= t (- time max-allowed-actions-lag))
          (doseq [act-fn! (aget pending-actions-map t)]
            (act-fn!)))
        (js-delete pending-actions-map t)
        (.shift pending-actions-times)
        (recur)))))

(defn- run-active-animations [time]
  (goog.object/forEach
   active-animations
   (fn [[t1 t2 an-fn! fin-fn!] aid _]
     (when (<= t1 time)
       (if (<= t2 time)
         (do
           (js-delete active-animations aid)
           (when fin-fn! (fin-fn!)))
         (when an-fn!
           (an-fn! time)))))))

(defn process-deffered-actions! []
  (doseq [da deffered-actions] (da))
  (set! deffered-actions (array)))

(defn process-animation! [time]
  (run-scheduled-actions time)
  (run-active-animations time)
  (set! last-animation-time time))

;; ---

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
    (let [p (.-position obj)
          [x2 y2] xy2]
      (set! (.-x p) x2)
      (set! (.-y p) y2))))

(defn- anim-linear-rotate-updater [obj t1 t2 angle1 angle2]
  (let [ai (mk-linear-interpolator t1 t2 angle1 angle2)]
    (fn [t]
      (set! (.-rotation obj) (ai t)))))

(defn- anim-infinite-linear-rotate-updater [obj c]
  (fn [t]
    (set! (.-rotation obj) (* t c))))

(defn- anim-linear-rotate-finisher [obj angle]
  (fn []
    (set! (.-rotation obj) angle)))

(defn linear-move! [aid obj t1 t2 xy1 xy2]
  (log :trace "linear-move" obj t1 t2 xy1 xy2)
  (add-animation!
   (if aid aid (str "lm-" (obj-uid obj)))
   t1
   t2
   (when xy1 (anim-linear-updater obj t1 t2 xy1 xy2))
   nil
   (anim-linear-finisher obj xy2)
   ))

(defn linear-rotate! [aid obj t1 t2 angle1 angle2]
  (log :trace "linear-rotate" obj t1 t2 angle1 angle2)
  (add-animation!
   (if aid aid (str "rot-" (obj-uid obj)))
   t1
   t2
   (when angle1 (anim-linear-rotate-updater obj t1 t2 angle1 angle2))
   nil
   (anim-linear-rotate-finisher obj angle2)
   ))

(defn infinite-linear-rotate! [aid obj c]
  (log :trace "inf-linear-rot" obj c)
  (add-animation!
   (if aid aid (str "rot-" (obj-uid obj)))
   1
   (.-MAX_VALUE js/Number)
   (anim-infinite-linear-rotate-updater obj c)
   ))
