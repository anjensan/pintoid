(ns pintoid.server.game
  (:use
   [pintoid.server utils math game-maps ecs])
  (:require
   [mount.core :refer [defstate]]
   [taoensso.timbre :as timbre]
   [pintoid.server.ecs :as ecs]))

;; -- api
(declare fix-world-state)
(declare game-remove-player)
(declare game-add-new-player)
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
(declare init-game-state)
(declare stop-world-agent)
(declare kill-entity)
(declare kill-player)
(declare is-colliding?)
(declare entity-out-of-gamefield?)
(declare search-new-player-pos)
(declare inc-player-score)

;; -- state

(def time-eid (next-entity-id))

(defstate last-stable-world :start (atom nil))
(defstate users-input :start (atom {}))
(defstate world
  :start (init-game-state (agent (create-ecs)))
  :stop (stop-world-agent world))


;; -- impl

(defn stop-world-agent [w]
  (timbre/debug "Stop world agent, world:" w)
  (send w (constantly ::destroyed)))

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
       (add-entity w eid (merge player-proto {:player true :position xy}))))))

(defn game-process-user-input [eid user-input]
  (swap! users-input assoc eid user-input))

(defn init-game-state [world]
  (send world sys-init-world-state (rand-nth game-maps)))

;; --

(defn sys-fixate-world-state [w]
  (reset! last-stable-world w))


(defn sys-init-world-state [w gm]
  (reduce add-new-entity w gm))


(defn sys-one-world-simulation-tick [w]
  (let [now (System/currentTimeMillis)]
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
      (sys-fixate-world-state))))


(defn run-world-simulation-tick []
  (send-off world sys-one-world-simulation-tick))


(defn sys-attach-world-time [w now]
  (add-entity w time-eid {:time now}))


(defn sys-kill-outdated-entities [w now]
  (transduce
   (filter #(<= (w % :sched-kill-at) now))
   (completing kill-entity)
   w
   (eids$ w :sched-kill-at)))


(defn sys-kill-entities-out-of-gamefield [w]
  (transduce
   (filter #(entity-out-of-gamefield? w %))
   (completing kill-entity)
   w
   (eids$ w :position)))


(defn sys-collide-entities [w]
    ;; TODO: optimize collision detect alg, currently it's O(n^2)!
  (let [;; TODO: use marker component :collidable or :collision-shape
        coll-eids (eids$ w [:* :radius :position])]
    (reduce
     (fn [w eid]
       (put-component
        w eid :collide-with
        (seq (filter #(is-colliding? w eid %) coll-eids))))
     w
     coll-eids)))


(defn sys-kill-collided-entities [w]
  (reduce
   (fn [w' eid]
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
           (as-> w' w
             (reduce inc-player-score w' bullets-owners)
             (kill-player w' eid)))

         ;; everything kills bullet except other bullets & players
         (and (= et :bullet) (not-any? #{:bullet :player} cwt))
         (kill-entity w' eid)

         :else w')))
   w
   (eids$ w :collide-with)))


(defn sys-physics-move [w dt]
  (into
   w
   (mapcat
    (fn [eid]
      (let [xy (w eid :position)
            fxy (w eid :fxy vector-0)
            m (w eid :mass 1)
            vxy (w eid :velocity vector-0)
            axy (vs* fxy (/ m))
            vxy' (v+ vxy (vs* axy dt))
            dt2 (/ dt 2)
            xy' (v+ xy (vs* vxy dt2) (vs* vxy' dt2))]
        [[eid :velocity vxy']
         [eid :position xy']])))
    (eids$ w [:* :position :phys-move [:+ :velocity :fxy]])))


(defn sys-physics-update-vxy [w dt]
  (into
   w
   (map
    (fn [eid]
      (let [xy (w eid :position)
            m (w eid :mass 1)
           fxy (reduce
                #(v+ %1 (calc-gravity-force m (w %2 :mass) xy (w %2 :position)))
                (w eid :self-fxy vector-0)
                (eids$ w [:- [:* :phys-act :position :mass] [eid]]))]
        [eid :fxy fxy])))
   (eids$ w [:* :position :mass :phys-move])))


(def sys-simulate-physics
  (make-timed-system
   (fn [w dt]
     (-> w
         (sys-physics-update-vxy dt)
         (sys-physics-move dt)))))


(defn sys-spawn-bullets [w now]
  (reduce
   (fn [w' eid]
     (or
      (let [ui (w eid :user-input)]
        (when (or (:fire? ui) (:alt-fire? ui))
          (let [b-proto (if (:fire? ui) bullet-proto bullet-alt-proto)
                bullet (:bullet b-proto)
                b-cooldown (:cooldown bullet)
                last-fire-at (w eid :last-fire-at)]
            (when (or (nil? last-fire-at) (< (+ last-fire-at b-cooldown) now))
              (let [xy (w eid :position)
                    vxy (w eid :velocity vector-0)
                    angle (w eid :angle)
                    b-vxy (v+ vxy (vas angle (:velocity bullet)))
                    b-xy xy
                    b-lifetime (:lifetime bullet)]
                (-> w'
                    (put-component eid :last-fire-at now)
                    (add-new-entity
                      (assoc b-proto
                        :position b-xy
                        :velocity b-vxy
                        :sched-kill-at (+ now b-lifetime)
                        :bullet (assoc bullet :owner eid)
                        :angle angle))))))))
      w'))
   w
   (eids$ w [:* :player :user-input])))


(defn sys-capture-users-input [w]
  (let [ui @users-input]
    (into
     w
     (for [eid (eids$ w :player)]
       [eid :user-input (get ui eid)]))))


(def sys-change-engine-based-on-ui
  (make-timed-system
   (fn [w dt]
     (into
      w
      (mapcat
       (fn [eid]
         (let [ui (w eid :user-input)
               rd (:rotate-dir ui 0)
               angle (w eid :angle 0)
               angle' (+ angle (* rd rotate-speed))
               ed (:engine-dir ui)
               ef (case ed -1 (- engine-reverse-force) 1 engine-forward-force 0)
               fxy (vas angle' ef)]
           [[eid :self-fxy fxy]
            [eid :angle angle']])))
      (eids$ w [:* :player :user-input])))))

;; --

(defn search-new-player-pos [w eid]
  (->Vector (rand-int 2000) (rand-int 2000)))


(defn inc-player-score [w eid]
  (if-let [{:keys [score] :as ps} (w eid :player)]
    (conj w [eid :score (inc (w eid :score 0))])))


(defn kill-player [w eid]
  (let [xy' (search-new-player-pos w eid)]
    (-> w
        (conj [eid :position xy'])
        (conj [eid :velocity nil])
        (conj [eid :deaths (inc (w eid :deaths 0))]))))


(defn entity-out-of-gamefield? [w eid]
  (when-let [xy (w eid :position)]
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
                 p1 (w e1 :position)
                 p2 (w e2 :position)]
      (< (dist p1 p2) (+ r1 r2)))))

(defn kill-entity [w eid]
  (if (w eid :player)
    ;; FIXME: use multimethods or protocols here
    (kill-player w eid)
    (drop-entity w eid)))
