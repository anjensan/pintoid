(ns pintoid.server.sys
  (:use [pintoid.server utils ecs]))

(defn- convert-seq-to-integrals
  [xs]
  (let [[nxs r]
        (reduce
         (fn [[s acc] x]
           (let [xx (+ x acc)
                 xxi (long xx)
                 xxd (- xx xxi)]
             (if (<= xxd 0.5)
               [(conj s xxi) xxd]
               [(conj s (inc xxi)) (- xxd 1)])))
         [[] 0]
         xs)]
    (when (seq nxs)
      (concat (butlast nxs)
              (vector (long (+ r (last nxs))))))))

(defn- quantize-time
  ([]
   (fn [w dt]
     [dt]))
  ([min-dt]
   (fn [w dt]
     (when (<= min-dt dt)
       [dt])))
  ([min-dt max-dt]
   (fn [w dt]
     (cond
       (> min-dt dt) []
       (<= min-dt dt max-dt) [dt]
       (zero? (rem dt max-dt)) (repeat (quot dt max-dt) max-dt)
       :else
       (let [n1 (int (/ dt max-dt))
             n2 (inc n1)
             n2dt (/ dt n2)]
         (if (<= min-dt n2dt max-dt)
           (convert-seq-to-integrals (repeat n2 n2dt))
           (repeat n1 max-dt)))))))

(defn timed-system
  ([sys-fn]
   (timed-system nil sys-fn))
  ([dt-quant sys-fn]
   (let [sid (next-entity :system)
         tq-fn (cond
                 (nil? dt-quant) (quantize-time)
                 (number? dt-quant) (quantize-time dt-quant)
                 (vector? dt-quant) (apply quantize-time dt-quant)
                 (fn? dt-quant) dt-quant)]
     (fn [w cur-time & rs]
       (let [prev-time (or (:last-time (w sid :system)) cur-time)
             dt (if prev-time (- cur-time prev-time) 0)
             dt-s (tq-fn w dt)
             target-time (reduce + prev-time dt-s)]
         (-> (if (== cur-time prev-time)
               w
               (reduce #(apply sys-fn %1 %2 rs) w dt-s))
             (add-entity sid {:system {:last-time target-time}
                              :type :system})))))))

(defn stateful-system
  ([sys-fn]
   (stateful-system nil sys-fn))
  ([initial-state sys-fn]
   (let [sid (next-entity :system)]
     (fn [w & rs]
       (let [state (w sid :system initial-state)
             [w' state'] (apply sys-fn w state)]
         (add-entity w' sid {:system state'
                               :type :system}))))))
