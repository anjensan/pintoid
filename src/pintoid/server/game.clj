(ns pintoid.server.game
  (:use [pintoid.server utils math game-maps ecs]))

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

;; -- internal fns
(declare kill-entity)
(declare kill-player)
(declare is-colliding?)
(declare entity-out-of-gamefield?)
(declare search-new-player-pos)
(declare inc-player-score)

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
          (sys-spawn-bullets now)
          (sys-change-engine-based-on-ui now)
          (sys-kill-outdated-entities now)
          (sys-simulate-physics now)
          (sys-collide-entities)
          (sys-kill-collided-entities)
          (sys-kill-entities-out-of-gamefield)
          (sys-attach-world-time now)
          (sys-fixate-world-state)
          ))))

(defn run-world-simulation-tick []
  (send-off world sys-one-world-simulation-tick))


(def sys-attach-world-time
  (fn [w now]
    (add-entity w time-eid {:time now})))

(def sys-kill-outdated-entities
  (system-each :sched-kill-at
   (fn [w eid now]
     (when (<= (w eid :sched-kill-at) now)
       (kill-entity w eid)))))

(def sys-kill-entities-out-of-gamefield
  (system-each :xy
   (fn [w eid]
     (when (entity-out-of-gamefield? w eid)
       (kill-entity w eid)))))

(def sys-collide-entities
  (fn [w]
    ;; TODO: optimize collision detect alg, currently it's O(n^2)!
    (let [;; TODO: use marker component :collidable or :collision-shape
          coll-eids (select-eids w [:* :radius :xy])]
      (reduce
       (fn [w eid]
         (put-component
          w eid :collide-with 
          (seq (filter #(is-colliding? w eid %) coll-eids))))
       w
       coll-eids))))

(def sys-kill-collided-entities
  (system-each
   :collide-with
   (fn [w eid]
     (let [et (w eid :type)
           cw (w eid :collide-with)
           cwt (map #(w % :type) cw)]

       ;; TODO: move out, use multimethods/protocols?
       (cond

        ;; everything kills player except own bullets
        (and (= et :player)
             (some #(not= eid (:owner (w % :bullet))) cw))
        (let [all-bullets-owners (keep #(:owner (w % :bullet)) cw)
              bullets-owners (remove #(= eid %) all-bullets-owners)]
          (as-> w w
                (reduce inc-player-score w bullets-owners)
                (kill-player w eid)))
        
        ;; everything kills bullet except other bullets & players
        (and (= et :bullet) (not-any? #{:bullet :player} cwt))
        (kill-entity w eid))

       ))))

(def sys-physics-move
  (system-each-into
   [:* :xy :phys-move [:+ :vxy :fxy]]
   (fn [w eid dt]
    (let [xy (w eid :xy)
          fxy (w eid :fxy vector-0)
          m (w eid :mass 1)
          axy (vs* fxy (/ m))
          vxy (w eid :vxy vector-0)
          vxy' (v+ vxy (vs* axy dt))
          dt2 (/ dt 2)
          xy' (v+ xy (vs* vxy dt2) (vs* vxy' dt2))]
      [[eid :vxy vxy']
       [eid :xy xy']]))))

(def sys-physics-update-vxy
  (system-each-into
   [:* :xy :mass :phys-move]
   (fn [w eid dt]
     (let [xy (w eid :xy)
           m (w eid :mass 1)
           fxy (reduce
                #(v+ %1 (calc-gravity-force m (w %2 :mass) xy (w %2 :xy)))
                (w eid :self-fxy vector-0)
                (select-eids w [:- [:* :phys-act :xy :mass] [eid]]))]
       [[eid :fxy fxy]]))))

(def sys-simulate-physics
  (system-timed
   (fn [w dt]
     (-> w
         (sys-physics-update-vxy dt)
         (sys-physics-move dt)))))

;; TODO: move to cs_comm.cljs

(defn point->vec [p]
  (when p
    ((juxt :x :y) p)))

(defn take-game-snapshot [w eid]
  {:player-eid eid
   :deaths (w eid :deaths 0)
   :score (w eid :score 0)
   :player-xy (point->vec (w eid :xy))
   })

(defn entitiy-upd-obj [w eid]
  {:eid eid
   :xy (point->vec (w eid :xy))
   :angle (w eid :angle)
   })

(defn entitiy-add-obj [w eid]
  {:eid eid
   :type (w eid :type)
   :xy (point->vec (w eid :xy))
   :angle (w eid :angle)
   :texture (w eid :texture)
   :dangle (w eid :dangle)
   })

(defn player-init-obj [eid ps]
  {:eid eid
   :type :player
   :xy (point->vec (:xy ps))
   :angle (:angle ps)
   })

(defn take-entities-snapshot [w eid client-eids]
  (let [;; TODO: send only coords of entities, compare with prev packet
        ;; entities (for [[eid es] (:entities g)] {:xy (:xy es)})
        all-eids (select-eids w :xy)              ; TODO: use special marker here
        cl-eids (eids client-eids)
        new-eids (seq (eids- all-eids cl-eids))
        upd-eids (seq (eids* all-eids cl-eids))
        rem-eids (seq (eids- cl-eids all-eids))
        ]
    {:upd (map #(entitiy-upd-obj w %) upd-eids)
     :add (map #(entitiy-add-obj w %) new-eids)
     :rem rem-eids}))

(def sys-spawn-bullets
  (system-each
   [:* :player :user-input]
   (fn [w eid now]
     (let [ui (w eid :user-input)]
       (when (or (:fire? ui) (:alt-fire? ui))
         (let [b-proto (if (:fire? ui) bullet-proto bullet-alt-proto)
               bullet (:bullet b-proto)
               b-cooldown (:cooldown bullet)
               last-fire-at (w eid :last-fire-at)]
           (when (or (nil? last-fire-at) (< (+ last-fire-at b-cooldown) now))
             (let [xy (w eid :xy)
                   vxy (w eid :vxy vector-0)
                   angle (w eid :angle)
                   b-vxy (v+ vxy (vas angle (:velocity bullet)))
                   b-xy xy
                   b-lifetime (:lifetime bullet)]
               (-> w
                   (put-component eid :last-fire-at now)
                   (add-new-entity
                    (assoc b-proto
                      :xy b-xy
                      :vxy b-vxy
                      :sched-kill-at (+ now b-lifetime)
                      :bullet (assoc bullet :owner eid)
                      :angle angle)))))))))))
     
(def sys-capture-users-input
  (fn [w]
    (let [ui @users-input]
      (into
       w
       (for [eid (select-eids w :player)]
         [eid :user-input (get ui eid)])))))
            

(def sys-change-engine-based-on-ui
  (system-timed
   (system-each-into
    [:* :player :user-input]
    (fn [w eid dt]
      (let [ui (w eid :user-input)
            rd (:rotate-dir ui 0)
            angle (w eid :angle 0)
            angle' (+ angle (* rd rotate-speed))
            ed (:engine-dir ui)
            ef (case ed -1 (- engine-reverse-force) 1 engine-forward-force 0)
            fxy (vas angle' ef)]
        [[eid :self-fxy fxy]
         [eid :angle angle']])))))

;; --

(defn search-new-player-pos [w eid]
  (->Vector (rand-int 2000) (rand-int 2000)))


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
    (let [[x y] ((juxt :x :y) xy)]
      (not
       (and
        (<= (- world-width) x world-width)
        (<= (- world-height) y world-height))))))


(defn is-colliding? [w e1 e2]
  ;; TODO: use multimethod here
   (when-not (== e1 e2)
    (when-some* [r1 (w e1 :radius)
                 r2 (w e2 :radius)
                 p1 (w e1 :xy)
                 p2 (w e2 :xy)]
      (< (dist p1 p2) (+ r1 r2)))))

(defn kill-entity [w eid]
  (if (w eid :player)
    ;; FIXME: use multimethods or protocols here
    (kill-player w eid)
    (drop-entity w eid)))
