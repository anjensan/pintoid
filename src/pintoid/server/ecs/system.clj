(ns pintoid.server.ecs.system
  (:use [pintoid.server.ecs core util]))

(defn run-stateful-system*
  ([sid w sf]
   (run-stateful-system* nil sf))
  ([sid w initial-state sf]
   (let [state (get-comp w sid ::system initial-state)
         [w' state'] (sf state)]
     (add-entity w' sid {::system state'}))))

(defmacro run-stateful-system
  ([w sf] (run-stateful-system w nil sf))
  ([w init sf] (let [sid (next-entity)] `(run-stateful-system* ~sid ~w ~init ~sf))))

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

(defn- quantize-time [dt-quant dt]
  (cond
    (nil? dt-quant)
    (when (> dt 0) [dt])
    (number? dt-quant)
    (when (<= dt-quant dt) [dt])
    (vector? dt-quant)
    (let [[min-dt max-dt] dt-quant]
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
            (repeat n1 max-dt)))))
    :else (throw (ex-info "invalid dt-quant"))))

(defn run-timed-system*
  ([sid w now sf]
   (run-timed-system* sid w now nil sf))
  ([sid w now dt-quant sf]
   (run-stateful-system* sid w now
    (fn [prev]
      (let [dt-s (quantize-time dt-quant (- now prev))
            ttime (reduce + prev dt-s)]
        [(reduce sf w dt-s) (reduce + prev dt-s)])))))

(defmacro run-timed-system
  ([w now sf] `(run-timed-system ~w ~now nil ~sf))
  ([w now dt sf] (let [sid (next-entity)] `(run-timed-system* ~sid ~w ~now ~dt ~sf))))

