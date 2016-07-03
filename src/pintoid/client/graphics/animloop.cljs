(ns pintoid.client.graphics.animloop
  (:require [goog.object] [goog.array])
  (:require-macros [pintoid.client.macros :refer [log]]))


;; completely skip outdated animations
(def max-allowed-actions-lag 5000)

;; animation-id => (array t1 t2 do-anim-fn! finish-anim-fn!)
(def active-animations (js-obj))
(def deffered-actions (array))
(def simple-active-animations (js-obj))

;; anim-start-time (t1) => array of [aid [t1 t2 anim-fn! finish-anim-fn!]]
(def pending-actions-map (array))

;; sorted list of keys from #'pending-actions-map
(def pending-actions-times (array))


(defn defer-action!
  ([act-fn!] (.push deffered-actions act-fn!))
  ([act-fn! & args] (defer-action! #(apply act-fn! args))))


(defn add-action!
  [t act-fn!]
  (let [tx (long t)
        al (aget pending-actions-map tx)]
    (if al
      (.push al act-fn!)
      (let [al' (array)]
        (aset pending-actions-map tx al')
        (.push al' act-fn!)
        (goog.array.binaryInsert pending-actions-times tx)))))


(defn add-animation!
  ([aid t1 t2 animate-fn!]
     (add-animation! aid t1 t2 animate-fn! nil nil))
  ([aid t1 t2 animate-fn! init-fn! finish-fn!]
   (let [av (array t1 t2 animate-fn! finish-fn!)]
     (log :trace "add penging animation" aid t1 t2)
     (add-action!
      t1
      (fn []
        (when init-fn! (init-fn!))
        (aset active-animations aid av))))))


(defn animate! [aid animate-fn!]
  (if animate-fn!
    (aset simple-active-animations aid animate-fn!)
    (js-delete simple-active-animations aid)))


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
   simple-active-animations
   (fn [an-fn! aid _]
     (an-fn! time)))
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


(defn run-deffered-actions! []
  (let [as deffered-actions]
    (set! deffered-actions (array))
    (run! #(%) as)))


(defn run-animations! [time]
  (run-scheduled-actions time)
  (run-active-animations time))
