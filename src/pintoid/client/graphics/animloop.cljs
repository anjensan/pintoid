(ns pintoid.client.graphics.animloop
  (:require
   [taoensso.timbre :as timbre :include-macros true]))


;; completely skip outdated animations
(def max-allowed-actions-lag 5000)

(def current-time 0)
(def active-animations (array))
(def pending-actions-map (array))
(def pending-actions-times (array))

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
  ([anim-fn]
   (.push active-animations anim-fn))
  ([t1 t2 anim-fn]
   (action! t1 (fn [] (animate! #(when (< % t2) (anim-fn %)))))))

(defn- run-scheduled-actions [time]
  (loop []
    (when-let [t (aget pending-actions-times 0)]
      (when (<= t time)
        (.shift pending-actions-times)
        (when (>= t (- time max-allowed-actions-lag))
          (let [al (aget pending-actions-map t)]
            (loop [i 0]
              (when (< i (.-length al))
                ((aget al i))
                (recur (inc i))))))
        (js-delete pending-actions-map t)
        (recur)))))

(defn- run-active-animations [time]
  (let [aa active-animations]
    (loop [i 0, k (alength aa)]
      (if (< i k)
        (let [f (aget aa i)]
          (if (f time)
            (recur (inc i) k)
            (do
              (aset aa i (aget aa (dec k)))
              (recur i (dec k)))))
        (set! (.-length aa) k)))))

(defn run-animations! [time]
  (set! current-time time)
  (run-scheduled-actions time)
  (run-active-animations time))
