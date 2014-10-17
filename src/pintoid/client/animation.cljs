(ns pintoid.client.animation
  (:require
   [pintoid.client.utils :refer [obj-uid]]))

;; -- API

(declare linear-move!)
(declare process-animations!)

;; -- animations

;; completely skip outdated animations
(def max-allowed-animation-lag 1000)
(def max-allowed-actions-lag 1000)

;; current (last) *animation* time
(def last-animation-time 0)

;; TODO: add init function?
;; animation-id => (array t1 t2 do-anim-fn! finish-anim-fn!)
(def active-animations (array))

;; anim-start-time (t1) => array of [aid [t1 t2 anim-fn! finish-anim-fn!]]
(def pending-actions-map (array))
(def pending-animaitons-map (array))

;; sorted list of keys from #'pending-animaitons-map
(def pending-actions-times (array))
(def pending-animaitons-times (array))


(defn add-action!
  [t act-fn!]
  (let [tx (long t)
        al (aget pending-actions-map tx)]
    (if al
      (.push al act-fn!)
      (let [al' (array)]
        (aset pending-actions-map tx al')
        (.push al' (array act-fn!))
        (.push pending-actions-times tx)
        (.sort pending-actions-times -)))))


(defn- add-pending-animation! [aid t avec]
  ;; FIXME: refactor, use #'add-action!
  (let [tx (long t)
        al (aget pending-animaitons-map tx)]
    (if al
      (do
        (.push al (array aid avec)))
      (let [al' (array)]
        (aset pending-animaitons-map tx al')
        (.push al' (array aid avec))
        (.push pending-animaitons-times tx)
        (.sort pending-animaitons-times -)))))


(defn add-animation!
  ([aid t1 t2 animate-fn!]
     (add-animation! aid t1 t2 animate-fn! nil))
  ([aid t1 t2 animate-fn! finish-fn!]
     (when (>= t2 last-animation-time)
       (let [av (array t1 t2 animate-fn! finish-fn!)
             aid (name aid)]
         (if (> t1 last-animation-time)
           (add-pending-animation! aid t1 av)
           (aset active-animations aid av))))))


(defn process-animations! [time]

  ;; (println time ">>>" pending-animaitons-times)

  ;; run pending actions
  (loop []
    (when-let [t (first pending-actions-times)]
      (when (<= t time)
        (when (>= t (- time max-allowed-actions-lag))
          (doseq [act-fn! (aget pending-actions-map t)]
            (act-fn!)))
        (js-delete pending-actions-map t)
        (.shift pending-actions-times)
        (recur))))

  ;; move some animations from pending state to active
  (loop []
    (when-let [t (first pending-animaitons-times)]
      (when (<= t time)
        (when (>= t (- time max-allowed-animation-lag))
          (doseq [aid-av (aget pending-animaitons-map t)]
            (let [aid (aget aid-av 0)
                  av (aget aid-av 1)]
              (aset active-animations aid av))))
        (js-delete pending-animaitons-map t)
        (.shift pending-animaitons-times)
        (recur))))
 
  ;; run active animations
  (doseq [aid (js/Object.keys active-animations)]
    (let [animation (aget active-animations aid)
          ;; [t1 t2 an-fn! fin-fn!] animation
          t1 (aget animation 0)
          t2 (aget animation 1)
          an-fn! (aget animation 2)
          fin-fn! (aget animation 3)]
      (when (<= t1 time)
        (if (<= t2 time)
          (do
            (js-delete active-animations aid)
            (when fin-fn! (fin-fn!)))
          (when an-fn!
            (an-fn! time))))))
 
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

