(ns pintoid.client.animation
  (:require
   [pintoid.client.utils :refer [obj-uid limit-str]])
  (:require-macros
   [pintoid.client.utils :refer [log-info log-debug]]))


;; -- API

(declare linear-move!)
(declare process-animation!)
(declare process-deffered-actions!)

;; -- animations

;; completely skip outdated animations
(def max-allowed-actions-lag 1000)

;; current (last) *animation* time
(def last-animation-time 0)

;; TODO: add init function?
;; animation-id => (array t1 t2 do-anim-fn! finish-anim-fn!)
(def active-animations (array))
(def deffered-actions (array))

;; anim-start-time (t1) => array of [aid [t1 t2 anim-fn! finish-anim-fn!]]
(def pending-actions-map (array))

;; sorted list of keys from #'pending-actions-map
(def pending-actions-times (array))


(defn defer-action!
  [act-fn!]
  (.push deffered-actions act-fn!))


(defn add-action!
  [t act-fn!]
  (log-debug "add action at" t ":" (limit-str 80 act-fn!))
  (let [tx (long t)
        al (aget pending-actions-map tx)]
    (if al
      (.push al act-fn!)
      (let [al' (array)]
        (aset pending-actions-map tx al')
        (.push al' (array act-fn!))
        (.push pending-actions-times tx)
        (.sort pending-actions-times -)))))


(defn- add-pending-animation! [aid t1 t2 avec]
  (add-action! t1 (fn [] (aset active-animations aid avec))))


(defn add-animation!
  ([aid t1 t2 animate-fn!]
     (add-animation! aid t1 t2 animate-fn! nil))
  ([aid t1 t2 animate-fn! finish-fn!]
     (log-debug "add animation" t1 "-" t2 (limit-str 80 animate-fn!))
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
  (doseq [animation active-animations]
    (let [;; [t1 t2 an-fn! fin-fn!] animation
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

(defn- anim-linear-updater
  [obj t1 t2 xy1 xy2]
  (let [t2-t1 (- t2 t1)
        p (.-position obj)
        [sx sy] xy1
        [dx dy] xy2
        ddc (/ 1 t2-t1)]
    (fn [time]
      (let [;; dd (/ (- t2 time) t2-t1)
            dd (* (- t2 time) ddc)
            sd (- 1 dd)
            x' (+ (* sx dd) (* dx sd))
            y' (+ (* sy dd) (* dy sd))]
        (set! (.-x p) x')
        (set! (.-y p) y')))))


(defn- anim-linear-finisher
  [obj xy2]
  (fn []
    (let [p (.-position obj)
          [dx dy] xy2]
      (set! (.-x p) dx)
      (set! (.-y p) dy))))


(defn linear-move! [aid obj t1 t2 xy1 xy2]
  (add-animation!
   (if aid (str "lm-" (obj-uid obj)))
   t1
   t2
   (when xy1 (anim-linear-updater obj t1 t2 xy1 xy2))
   (anim-linear-finisher obj xy2)
   ))

