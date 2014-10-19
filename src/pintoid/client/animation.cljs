(ns pintoid.client.animation
  (:require
   [pintoid.client.utils :refer [obj-uid limit-str]])
  (:require-macros
   [pintoid.client.utils :refer [log]]))


;; -- API

(declare linear-move!)
(declare process-animation!)
(declare process-deffered-actions!)

;; -- animations

;; completely skip outdated animations
(def max-allowed-actions-lag 1000)

; (def initial-player-angle ((/ (.PI js/Math) 2))

;; current (last) *animation* time
(def last-animation-time 0)

;; TODO: add init function?
;; animation-id => (array t1 t2 do-anim-fn! finish-anim-fn!)
(def active-animations (js-obj))
(def deffered-actions (array))

;; anim-start-time (t1) => array of [aid [t1 t2 anim-fn! finish-anim-fn!]]
(def pending-actions-map (js-obj))

;; sorted list of keys from #'pending-actions-map
(def pending-actions-times (array))


(defn defer-action!
  [act-fn!]
  (.push deffered-actions act-fn!))


(defn add-action!
  [t act-fn!]
  (log :trace "add action at" t ":" (limit-str 80 act-fn!))
  (let [tx (long t)
        al (aget pending-actions-map tx)]
    (if al
      (.push al act-fn!)
      (let [al' (array)]
        (aset pending-actions-map tx al')
        (.push al' act-fn!)
        (.push pending-actions-times tx)
        (.sort pending-actions-times -)))))


(defn- add-pending-animation! [aid t1 t2 avec]
  (log :trace "add penging animation" aid t1 t2)
  (add-action! t1 (fn [] (aset active-animations aid avec))))


(defn add-animation!
  ([aid t1 t2 animate-fn!]
     (add-animation! aid t1 t2 animate-fn! nil))
  ([aid t1 t2 animate-fn! finish-fn!]
     (when (>= t2 last-animation-time)
       (let [aid (name aid)
             av (array aid t1 t2 animate-fn! finish-fn!)]
         (if (> t1 last-animation-time)
           (add-pending-animation! aid t1 t2 av)
             (aset active-animations aid av))))))

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
  (doseq [aid (js/Object.keys active-animations)]
    (let [animation (aget active-animations aid)
          ;; [t1 t2 an-fn! fin-fn!] animation
          aid (aget animation 0)
          t1 (aget animation 1)
          t2 (aget animation 2)
          an-fn! (aget animation 3)
          fin-fn! (aget animation 4)]
      (when (<= t1 time)
        (if (<= t2 time)
          (do
            (js-delete active-animations aid)
            (when fin-fn! (fin-fn!)))
          (when an-fn!
            (an-fn! time)))))))


(defn process-deffered-actions! []
  (doseq [x deffered-actions]
    (x))
  (set! (.-length deffered-actions) 0))


(defn process-animation! [time]
  (run-scheduled-actions time)
  (run-active-animations time)
  (set! last-animation-time time))


;; linear moving
(defn linear-inter [t1 t2 v1 v2]
  (let [
    t2-t1 (- t2 t1)
    ddc (/ 1 t2-t1)
    ]
    (fn [time]
      (let [;; dd (/ (- t2 time) t2-t1)
            dd (* (- t2 time) ddc)
            sd (- 1 dd)
            x' (+ (* v1 dd) (* v2 sd))]
            x'))))

(defn- anim-linear-updater
  [obj t1 t2 xy1 xy2]
  (let [t2-t1 (- t2 t1)
        p (.-position obj)
        [sx sy] xy1
        [dx dy] xy2
        ddc (/ 1 t2-t1)]
    (fn [time]
      (let [;; dd (/ (- t2 time) t2-t1)
           
            x' ((linear-inter t1 t2 sx dx) time)
            y' ((linear-inter t1 t2 sy dy) time)]
        (set! (.-x p) x')
        (set! (.-y p) y')))))


(defn- anim-linear-finisher
  [obj xy2]
  (fn []
    (let [p (.-position obj)
          [dx dy] xy2]
    rot(set! (.-x p) dx)
      (set! (.-y p) dy))))


(defn- anim-linear-rotate-updater [obj t1 t2 angle1 angle2]
    (fn [time]
        (set! (.-rotation obj) ((linear-inter t1 t2 angle1 angle2) time))
        ))

(defn- anim-infinite-linear-rotate-updater [obj c]
    (fn [time]
        (println "ANGLES" angle1 angle2 time)

        (set! (.-rotation obj) (* time c))
        ))



(defn- anim-linear-rotate-finisher [obj angle]
  (fn []
    (set! (.-rotation obj) angle)
    ))



(defn linear-move! [aid obj t1 t2 xy1 xy2]
  (log :trace "linear-move" obj t1 t2 xy1 xy2)
  (add-animation!
   (if aid aid (str "lm-" (obj-uid obj)))
   t1
   t2
   (when xy1 (anim-linear-updater obj t1 t2 xy1 xy2))
   (anim-linear-finisher obj xy2)
   ))

(defn linear-rotate! [aid obj t1 t2 angle1 angle2]
  (println "ROTATION" angle1 angle2)
  (add-animation!
   (if aid aid (str "rot-" (obj-uid obj)))
   t1
   t2
   (when angle1 (anim-linear-rotate-updater obj t1 t2 angle1 angle2))
   (anim-linear-rotate-finisher obj angle2)
   ))

(defn infinite-linear-rotate! [aid obj c]

  (add-animation!
   (if aid aid (str "rot-" (obj-uid obj)))
   1
   (.-MAX_VALUE js/Number)
   (anim-infinite-linear-rotate-updater obj c)
   (anim-linear-rotate-finisher obj 0)
   ))

