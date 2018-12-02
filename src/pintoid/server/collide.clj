(ns pintoid.server.collide
  (:use
   [pintoid.ecs core system entity])
  (:require
   [pintoid.server.vec2 :as v2]
   [taoensso.timbre :as timbre]))

(defmulti mbr
  (fn [w eid] (get-comp w eid :collide)))

(defmulti collide?
  (fn [w e1 e2]
    [(get-comp w e1 :collide)
     (get-comp w e2 :collide)]))

(defn- detect-collisions-naive-1 [w e eids acc]
  (loop [[z & r] eids, a acc]
    (if z
      (if (collide? w e z)
        (recur r (cons [e z] a))
        (recur r a))
      a)))

(defn detect-collisions-naive [w eids]
  (loop [[e & r] (seq eids), acc nil]
    (if e
      (recur r (detect-collisions-naive-1 w e r acc))
      acc)))

(defn detect-collisions-nwl [w eids]
  (let [ems (->> eids
                 (map (fn [e] [e (mbr w e)]))
                 (sort-by (fn [[_ [a]]] (:x a))))]
    (second
     (reduce
      (fn [[ws acc] [e [a b] :as em]]
        (let [{xa :x ya :y} a
              {yb :y} b
              ws'(filter (fn [[e1 [_ b1]]] (>= (:x b1) xa)) ws)]
          [(cons em ws')
           (into acc
            (comp
             (filter (fn [[e1 [a1 b1]]]
                       (and
                        (<= (max ya (:y a1)) (min yb (:y b1)))
                        (collide? w e e1))))
             (map (fn [[e1 _]] [e e1])))
            ws)]))
      [() ()]
      ems))))

(defn- detect-collisions-auto [w eids]
  (let [n (count eids)]
    (cond
      (<= n 1) nil
      (<= n 5) (detect-collisions-naive w eids)
      :else (detect-collisions-nwl w eids))))

(defn- mbr-max-size [w e]
  (let [[{x1 :x y1 :y} {x2 :x y2 :y}] (mbr w e)]
    (max (- y2 y1) (- x2 x1))))

(defn detect-collisions [w eids]
  (let [mbrs (mapv #(let [[a b] (mbr w %)]
                      [% (:x a) (:x b) (:y a) (:y b)])
                   eids)
        ccz (transduce
             (map (fn [[_ x1 x2 y1 y2]] (max (- x2 x1) (- y2 y1))))
             (completing max)
             1
             mbrs)
        cm (persistent!
            (reduce
             (fn [cm [e x1 x2 y1 y2]]
               (let [cx1 (long (/ x1 ccz))
                     cy1 (long (/ y1 ccz))
                     cx2 (long (/ x2 ccz))
                     cy2 (long (/ y2 ccz))]
                 (loop [x cx1 cm cm]
                   (if (<= x cx2)
                     (recur
                      (inc x)
                      (loop [y cy1 cm cm]
                        (if (<= y cy2)
                          (recur
                           (inc y)
                           (let [h [x y]] (assoc! cm h (cons e (get cm h)))))
                          cm)))
                     cm))))
             (transient {})
             mbrs))]
    (mapcat (fn [[_ eids]] (detect-collisions-auto w eids)) cm)))

(defn- mergein-collide-with-comp! [w css]
  (reduce
   (fn
     ([w'] w')
     ([w' [a b]]
      (-> w'
          (assoc! a (conj (get w' a #{}) b))
          (assoc! b (conj (get w' b #{}) a)))))
   w css))

(defn- drop-collide-with-comp! [w]
  (reduce #(drop-comp! %1 %2 :collide-with)
          w (entities w :collide-with)))

(defn- put-collide-with-comp! [w d]
  (reduce (fn [w' [e cw]] (put-comp! w' e :collide-with cw)) w d))

(defn asys-generic-collide-entities [w cf]
  (let [d (-> (transient {})
              (mergein-collide-with-comp! (cf w (entities w :collide)))
              (persistent!))]
    (timbre/tracef "Detected %d collisions" (count d))
    (fn->
      (transient)
      (drop-collide-with-comp!)
      (put-collide-with-comp! d)
      (persistent!))))

(defn asys-collide-entities [w]
  (asys-generic-collide-entities w detect-collisions))

;; ===

(defmethod mbr :circle [w eid]
  (let-entity w eid [p :position, r :radius]
    (let [rv (v2/vec2 r r)]
      [(v2/v- p rv)
       (v2/v+ p rv)])))

(defmethod collide? [:circle :circle] [w e1 e2]
  (let-entity w e1 [p1 :position, r1 :radius]
    (let-entity w e2 [p2 :position, r2 :radius]
      (<= (v2/dist p1 p2) (+ r1 r2)))))


