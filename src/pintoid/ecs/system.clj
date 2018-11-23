(ns pintoid.ecs.system
  (:use [pintoid.ecs core util]))

(defmacro fn-> [& forms]
  `(fn [w#] (-> w# ~@forms)))

(defn run-stateful-system*
  ([sid w sf]
   (run-stateful-system* nil sf))
  ([sid w init sf]
   (let [state (get-comp w sid ::system init)
         [aw state'] (sf state)]
     (comp
      (fn-> (add-entity sid {::system state'}))
      aw))))

(defmacro run-stateful-system [w init sf]
   (let [sid (next-entity)]
     `(run-stateful-system* ~sid ~w ~init ~sf)))

(defn run-timed-system* [sid w now sf]
  (run-stateful-system*
   sid w now
   (fn [prev]
     (let [dt (- now prev)]
       [(sf dt) now]))))

(defmacro run-timed-system [w now sf]
  (let [sid (next-entity)]
    `(run-timed-system* ~sid ~w ~now ~sf)))

(defn combine-systems [fs]
  (let [fs (into [] (filter some?) fs)]
    (fn [w]
      (reduce (fn [w' f] (or (f w') w')) w fs))))

(defn combine-systems! [fs!]
  (let [fs! (into [] (filter some?) fs!)]
    (fn [w]
      (persistent!
       (reduce (fn [w' f!] (or (f! w') w')) (transient w) fs!)))))

(defn asys-fork-join []
  (let [a (atom {})]
    [(fn fork [w id as & args]
       (let [f (future (apply as w args))]
         (swap! a assoc id f))
       w)
     (fn join [w id]
       (let [f (get @a id)]
         (@f w)))]))
