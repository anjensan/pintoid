(ns pintoid.server.game
  (:use [pintoid.server utils physics game-maps ecs]))

;; -- api
(declare fix-world-state)
(declare init-world-state)
(declare game-remove-player)
(declare game-add-new-player)
(declare take-game-snapshot)
(declare run-world-simulation-tick)

;; -- various systems
(declare sys-attach-world-time)
(declare sys-init-world-state)
(declare sys-collide-entities)
(declare sys-kill-outdated-entities)
(declare sys-kill-collided-entities)
(declare sys-kill-entities-out-of-gamefield)
(declare sys-simulate-physics)
(declare sys-fixate-world-state)
(declare sys-spawn-bullets)
(declare sys-capture-users-input)
(declare sys-change-engine-based-on-ui)

(declare kill-entity)
(declare kill-player)
(declare is-colliding?)
(declare entity-out-of-gamefield?)
(declare search-new-player-pos)

;; -- state

(def last-stable-world (atom nil))
(def users-input (atom {}))
(def time-eid (next-entity-id))
(def world (agent (create-ecs)))


;; -- impl

(defn fix-world-state []
  (or @last-stable-world @world))

(defn get-world-time [w]
  (w time-eid :time))

(defn game-remove-player [eid]
  (send world drop-entity eid))

(defn game-add-new-player [eid]
  (send
   world
   (fn [w]
     (let [xy (search-new-player-pos w eid)]
       (-> w
           (add-entity eid player-proto)
           (put-component eid :xy xy)))))
  (await world)
  (entity @world eid))

(defn game-process-user-input [eid user-input]
  (swap! users-input assoc eid user-input))

(defn init-world-state []
  (send world sys-init-world-state (rand-nth game-maps)))

;; --

(def sys-fixate-world-state
  (fn [w]
    (reset! last-stable-world w)))

(def sys-init-world-state
  (fn [w gm]
    (reduce add-new-entity w gm)))

(def sys-one-world-simulation-tick
  (fn [w]
    (let [now (current-os-time)]
      (-> w
          (sys-capture-users-input)
          (sys-change-engine-based-on-ui)
          (sys-spawn-bullets)
          (sys-kill-outdated-entities now)
          (sys-simulate-physics now)
          (sys-collide-entities)
          ;;(sys-kill-collided-entities)
          ;;(sys-kill-entities-out-of-gamefield)
          (sys-fixate-world-state)
          (sys-attach-world-time now)
          ))))

(defn run-world-simulation-tick []
  (send-off world sys-one-world-simulation-tick))


(def sys-attach-world-time
  (fn [w now]
    (add-entity w time-eid {:time now})))

(def sys-kill-outdated-entities
  (fn [w now]
    (reduce
     (fn [w eid]
       (if (<= (w eid :sched-kill-at) now)
         (kill-entity w eid)
         w))
     w
     (eids$ w :sched-kill-at))))

(def sys-kill-entities-out-of-gamefield
  (fn [w]
    (reduce
     (fn [w eid]
       (if (entity-out-of-gamefield? w eid)
         (kill-entity w eid)
         w))
     w
     (eids$ w :xy))))

(def sys-collide-entities
  (fn [w]
    ;; TODO: optimize collision detect alg, currently it's O(n^2)!
    (let [;; TODO: use marker component :collidable or :collision-shape
          coll-eids (eids$ w [:* :radius :xy])]
      (reduce
       (fn [w eid]
         (put-component
          w eid :collide-with 
          (filterv #(is-colliding? w eid %) coll-eids)))
       w
       coll-eids))))


(def sys-kill-collided-entities
  (fn [w]
    w))

(def phys-move-entity-seq
  (fn [w eid dt]
    (let [xy (w eid :xy)
          fxy (w eid :fxy [0 0])
          m (w eid :mass 1)
          axy (vs* fxy (/ m))
          vxy (w eid :vxy [0 0])
          vxy' (v+ vxy (vs* axy dt))
          dt2 (/ dt 2)
          xy' (v+ xy (v+ (vs* vxy dt2) (vs* vxy' dt2)))]
      [[eid :vxy vxy'] [eid :xy xy']])))

(def phys-update-enity-fxy-seq
  (fn [w eid dt]
    (let [xy (w eid :xy [0 0])
          m (w eid :mass 1)
          fxy (reduce
              #(v+ %1 (calc-gravity-force m (w %2 :mass) xy (w %2 :xy)))
              (w eid :self-fxy [0 0])
              (eids$ w [:- [:* :phys-act :xy :mass] [eid]]))]
      [[eid :fxy fxy]])))

(def sys-physics-move
  (system-each-into phys-move-entity-seq [:* :xy :phys-move [:+ :vxy :fxy]]))

(def sys-physics-update-vxy
  (system-each-into phys-update-enity-fxy-seq [:* :xy :mass :phys-move]))

(def sys-simulate-physics
  (system-timed
   (fn [w dt]
     (-> w
         (sys-physics-update-vxy dt)
         (sys-physics-move dt)))))


(defn take-game-snapshot [w eid]
  {:player-eid eid
   :deaths (:deaths (w eid :player) 0)
   :score (:deaths (w eid :player) 0)
   :player-xy (w eid :xy)
   })

(defn entitiy-upd-obj [w eid]
  {:eid eid
   :xy (w eid :xy)
   :angle (w eid :angle)
   })

(defn take-entities-snapshot [w eid client-eids]
  (let [;; TODO: send only coords of entities, compare with prev packet
        ;; entities (for [[eid es] (:entities g)] {:xy (:xy es)})
        all-eids (eids$ w :xy)              ; TODO: use special marker here
        cl-eids (eids client-eids)
        new-eids (seq (eids- all-eids cl-eids))
        upd-eids (seq (eids* all-eids cl-eids))
        rem-eids (seq (eids- cl-eids all-eids))
        ]
    {:upd (map #(entitiy-upd-obj w %) upd-eids)
     :add (map #(assoc (entity w %) :eid %) new-eids)
     :rem rem-eids}))


(def sys-spawn-bullets
  (fn [w]
    w))

(def sys-capture-users-input
  (fn [w]
    (let [ui @users-input]
      (into
       w
       (for [eid (eids$ w :player)]
         [eid :user-input (get ui eid)])))))
            

(def sys-change-engine-based-on-ui
  (system-each-into
   (fn [w eid]
     (let [ui (w eid :user-input)
           a (:angle ui 0)
           ed (:engine-dir ui)
           ef (case ed -1 (- engine-reverse-force) 1 engine-forward-force 0)
           fxy (if (zero? ef) [0 0] (vas a ef))]
       [[eid :self-fxy fxy]
        [eid :angle a]]))
   [:* :player :user-input]))


;; --

(defn search-new-player-pos [w eid]
  [(rand-int 2000) (rand-int 2000)])


(defn inc-player-score [w eid]
  (if-let [{:keys [score] :as ps} (w eid :player)]
    (conj w [eid :score (inc (w eid :score 0))])))


(defn kill-player [w eid]
  (let [xy' (search-new-player-pos w eid)]
    (-> w
        (conj [eid :xy xy'])
        (conj [eid :vxy nil])
        (conj [eid :deaths (inc (w eid :deaths 0))]))))


(defn entity-out-of-gamefield? [w eid]
  (when-let [xy (w eid :xy)]
    (let [[x y] xy]
      (or
       (< x (- world-width))
       (> x world-width)
       (< y (- world-height))
       (> y world-height)))))


(defn is-colliding? [w e1 e2]
  ;; TODO: use multimethod here
   (when (not= e1 e2)
    (when-some* [r1 (w e1 :radius)
                 r2 (w e2 :radius)
                 xy1 (w e1 :xy)
                 xy2 (w e2 :xy)]
      (< (distance2 xy1 xy2) (sqr (+ r1 r2))))))

(defn kill-entity [w eid]
  (if (w eid :player)
    ;; FIXME: use multimethods or protocols here
    (kill-player w eid)
    (drop-entity w eid)))
