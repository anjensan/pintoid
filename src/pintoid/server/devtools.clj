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
            ['drawRect x1 y1 (- x2 x1) (- y2 y1)]
            ]})))

(defn- create-arrow-sprite
  [{:keys [cid
           scale
           color
           line-width
           line-alpha
           arrow-size
           arrow-angle
           ]
    :or {scale 1,
         color 0xFFFFFF,
         line-width 3,
         line-alpha 0.75
         arrow-size 16
         arrow-angle 2.7
         }}
   w eid]
  (when-let [p (get-comp w eid cid)]
    (let [p (v2/scale p scale)
          r #(int (+ % 0.5))
          a (v2/scale (v2/norm p) (min arrow-size (v2/mag p)))
          a1 (v2/rotate a arrow-angle)
          a2 (v2/rotate a (- arrow-angle))
          {xc1 :x, yc1 :y} (v2/v+ p a1)
          {xc2 :x, yc2 :y} (v2/v+ p a2)
          {x :x, y :y} p]
      {:type :graphics
       :layer devtools-layer
       :angle 0
       :do [
            ['lineStyle line-width color line-alpha]
            ['moveTo 0 0]
            ['lineTo (r x) (r y)]
            ['lineTo (r xc1) (r yc1)]
            ['moveTo (r x) (r y)]
            ['lineTo (r xc2) (r yc2)]
            ]})))

(defn make-visualize-component-asys [uid query make-sprite]
  (fn [w enabled]
    (comp
     (if-not enabled
       identity
       (combine-systems!
        (for [eid (apply entities w query)
              :let [ss (make-sprite w eid)]
              :when ss]
          (fn->
           (put-comp! eid uid ())
           (update-comp! eid :sprite assoc-ex uid ss)))))
     (combine-systems!
      (for [eid (entities w uid)]
        (fn->
         (drop-comp! eid uid)
         (update-comp! eid :sprite dissoc uid))))
     #(load-entity-from-var % #'devtools-layer)
     )))

(def asys-show-collision-mbrs
  (make-visualize-component-asys
   ::mbr [:collide]
   create-mbr-sprite))

(def asys-show-velocity
  (make-visualize-component-asys
   ::velocity [:velocity]
   (partial #'create-arrow-sprite {:cid :velocity, :scale 100, :color 0xCCFF44})))

(def asys-show-force
  (make-visualize-component-asys
   ::force [:force]
   (partial #'create-arrow-sprite {:cid :force, :scale 1500, :color 0xFF77FF})))
