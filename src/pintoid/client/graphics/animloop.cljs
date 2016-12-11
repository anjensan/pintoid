(ns pintoid.client.graphics.animloop
  (:require
   [goog.object]
   [goog.array]
   [taoensso.timbre :as timbre :include-macros true]))


;; completely skip outdated animations
(def max-allowed-actions-lag 5000)

(def active-animations (js-obj))
(def simple-active-animations (js-obj))

(def current-time 0)
(def pending-actions-map (array))
(def pending-actions-times (array))

(deftype ActiveAnimation [t1 t2 animfn finishfn])


(defn action! [t act-fn]
  (if (>= current-time t)
    (act-fn)
    (let [tx (long t)
          al (aget pending-actions-map tx)]
      (if al
        (.push al act-fn)
        (let [al' (array)]
          (.push al' act-fn)
          (aset pending-actions-map tx al')
          (goog.array/binaryInsert pending-actions-times tx))))))

(defn animate!
  ([aid anim-fn]
   (if anim-fn
     (aset simple-active-animations aid anim-fn)
     (js-delete simple-active-animations aid)))
  ([aid t1 t2 anim-fn]
     (animate! aid t1 t2 anim-fn nil nil))
  ([aid t1 t2 anim-fn init-fn finish-fn]
   (let [av (ActiveAnimation. t1 t2 anim-fn finish-fn)]
     (timbre/trace "add penging animation" aid t1 t2)
     (action! t1
      (fn []
        (when init-fn (init-fn))
        (aset active-animations aid av))))))

(defn- execute-fns-array [al]
  (loop [i 0]
    (when (< i (.-length al))
      ((aget al i))
      (recur (inc i)))))

(defn- run-scheduled-actions [time]
  (loop []
    (when-let [t (aget pending-actions-times 0)]
      (when (<= t time)
        (.shift pending-actions-times)
        (when (>= t (- time max-allowed-actions-lag))
          (execute-fns-array (aget pending-actions-map t))
        (js-delete pending-actions-map t)
        (recur))))))

(defn- run-active-animations [time]
  (goog.object/forEach
   simple-active-animations
   (fn [an-fn! _ _]
     (an-fn! time)))
  (goog.object/forEach
   active-animations
   (fn [aa aid _]
     (when (<= (.-t1 aa) time)
       (if (<= (.-t2 aa) time)
         (do
           (js-delete active-animations aid)
           (when-let [ff (.-finishfn aa)] (ff)))
         (when-let [af (.-animfn aa)] (af time)))))))

(defn run-animations! [time]
  (set! current-time time)
  (run-scheduled-actions time)
  (run-active-animations time))
