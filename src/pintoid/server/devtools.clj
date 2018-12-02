(ns pintoid.server.devtools
  (:use
   [pintoid.ecs core system entity]
   [pintoid.server collide])
  (:require
   [pintoid.server.vec2 :as v2]
   [taoensso.timbre :as timbre]))

(defasset devtools-layer :layer
  {:zorder 1000})

(defn- assoc-ex [s k m]
  (assoc (if (map? s) s {nil s}) k m))

(defn- create-mbr-sprite [w eid]
  (let-entity w eid [p :position]
    (let [[a1 a2] (mbr w eid)
          {x1 :x y1 :y} (v2/v- a1 p)
          {x2 :x y2 :y} (v2/v- a2 p)
          cl (if (seq (get-comp w eid :collide-with))
               0xff3333 0x22ff22)]
      {:type :graphics
       :layer devtools-layer
       :angle 0
       :do [['lineStyle 3 cl 0.75]
            ;; ['beginFill cl 0.1]
            ['drawRect x1 y1 (- x2 x1) (- y2 y1)]
            ]})))

(defn make-visualize-component-asys [uid query make-sprite]
  (fn [w enabled]
    (if enabled
      (comp
       #(load-entity-from-var % #'devtools-layer)
       (combine-systems
        (for [eid (apply entities w query)
              :let [ss (make-sprite w eid)]
              :when ss]
          (fn->
           (put-comp eid uid :enabled)
           (update-comp eid :sprite assoc-ex uid ss)))))
      (combine-systems
       (for [eid (entities w uid)]
         (fn->
          (drop-comp eid uid)
          (update-comp eid :sprite dissoc uid)))))))

(def asys-show-collision-mbrs
  (make-visualize-component-asys ::mbr [:collide] create-mbr-sprite))
