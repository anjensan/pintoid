(ns pintoid.server.ecs.system
  (:use [pintoid.server.ecs core util]))

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

(defn comp-system [fs]
  (let [fs (into [] (filter some?) fs)]
    (fn [w]
      (reduce (fn [w' f] (or (f w') w')) w fs))))

(defn comp-system! [fs!]
  (let [fs! (into [] (filter some?) fs!)]
    (fn [w]
      (persistent!
       (reduce (fn [w' f!] (or (f! w') w')) (transient w) fs!)))))

(defn asys->sys [sf]
  (fn [w & args]
    ((apply sf w args) w)))
