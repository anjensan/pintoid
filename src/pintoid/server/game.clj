(ns pintoid.server.game
  (:use [pintoid.server utils physics game-maps])
  (:require
   [clojure.core.async :refer
    [<! >! put! close! go-loop go timeout]]))

;; API

(declare fix-world-state)
(declare create-entities-snapshot)
(declare schedule-world-simulation)

;; -- consts

(def world
  (agent
   {
    :at 0
    :entities {}                        ; map of active entities
    }))


(declare move-entity)
(declare add-new-entity)
(declare search-new-player-pos)
(declare kill-player)
(declare is-colliding?)

(declare search-new-player-pos)
(declare kill-player)
(declare entity-out-of-gamefield?)
(declare maybe-collide-entity-with-something)


(defn fix-world-state []
  @world)


(defn current-time []
  (System/currentTimeMillis))


(defmacro update-world!
  [s & body]
  `(send world (fn ~s ~@body)))


(defmacro world->!
  [& body]
  `(update-world! [w#] (-> w# ~@body)))


;; --


(defn init-world-state []
  (let [gm (rand-nth game-maps)]
    (update-world! [w]
     (reduce
      add-new-entity
      (assoc w :at (current-time))
      (map (partial merge default-entity) gm)))))


(declare update-entities-physics)
(declare remove-outdated-entities)
(declare kill-out-of-gamefield-of-entities)
(declare kill-colliding-entities)


(defn run-world-simulation-tick []
  (update-world! [w]
    (let [t1 (:at w)
          t2 (current-time)]
      (-> w
          (remove-outdated-entities t2)
          (update-entities-physics t1 t2)
          (kill-out-of-gamefield-of-entities t2)
          (kill-colliding-entities t2)
          (assoc :at t2)
          ))))


(defn remove-outdated-entities [w t2]
  (let [ess (:entities w)
        ess' (into
              {}
              (remove
               (fn [[eid es]] (when-let [t (:drop-at es)] (<= t t2)))
               ess))]
    (assoc w :entities ess')))


(defn kill-out-of-gamefield-of-entities [w t2]
  (let [kes (filter
             (partial entity-out-of-gamefield? w)
             (vals (:entities w)))
        kill (fn [w es]
               (let [eid (:eid es)]
                 (if (= (:type es) :player)
                   (kill-player w eid)
                   (assoc w :entities (dissoc (:entities w) eid)))))]
  (reduce kill w kes)))


(defn kill-colliding-entities [w t2]
  (reduce maybe-collide-entity-with-something w (vals (:entities w))))


(defn update-entity-physics-position [entity phobjs t1 t2]
  (if-not (:phys-move entity)
    entity
    (let [eid (:eid entity)
          vxy (:vxy entity [0 0])
          xy (:xy entity)
          pxy (or (:pxy entity) (v+ xy (vs* vxy (- t1 t2))))
          m (or (:mass entity) 1)
          fc (reduce v+ (:fxy entity [0 0])
                     (map #(if (= (:eid %) eid)
                             [0 0]
                             (calc-gravity-force m (:mass %) xy (:xy %)))
                          phobjs))
          dt (- t2 t1)
          dxy (hardlimit-force-2d (vs* fc (/ 1 m)))
          xy' (integrate-verle-2d pxy xy dxy dt)
          vxy' (when (not= t1 t2) (vs* (v- xy' xy) (/ 1 (- t2 t1))))]
      (assoc
          entity
        :pxy xy
        :xy xy'
        :vxy vxy'                        ; used by spawn-bullet
        ))))


(defn update-entities-physics [w t1 t2]
  (let [es (:entities w)
        phobjs (filter :phys-act (vals es))
        upd-ent (fn [[eid e]]
                  [eid (update-entity-physics-position e phobjs t1 t2)])]
    (assoc w :entities (into {} (map upd-ent es)))))


(defn add-new-entity
  [w estate]
  (let [eid (or (:eid estate) (next-eid))
        es (assoc estate :eid eid)]
    (assoc-in w [:entities eid] es)))


(defn game-remove-player [eid]
  (update-world!
   [w]
   (-> w
       ;; ADD transition state for entitie - BLOW IT!
       (update-in [:entities] dissoc eid))))


(defn game-add-new-player
  [eid]
  (let [pa (atom 0)]
    (update-world!
     [w]
     (let [xy (search-new-player-pos w eid)
           ps (assoc player-proto :eid eid :xy xy)]
       (reset! pa ps)
       (add-new-entity w ps)))
    (await world)
    @pa))


(defn take-game-snapshot [g eid]
  {:player-eid eid
   :deaths (or (get-in g [:entities eid :deaths]) 0)
   :score (or (get-in g [:entities eid :score]) 0)
   :player-xy (get-in g [:entities eid :xy])
   })


(defn entitiy-upd-obj [entity]
  {:eid (:eid entity)
   :xy (:xy entity)
   :angle (:angle entity)
   ;; TODO: add 
   })


(defn- abs [x]
  (if (< x 0) (- x) x))


(defn user-can-view-entity [ps es]
  (let [xy1 (:xy ps)
        xy2 (:xy es)]
    (when (and xy1 xy2)
      (let [[x1 y1] xy1
            [x2 y2] xy2
            d (+ (abs (- x1 x2)) (abs (- y1 y2)))]
        (<= d max-user-view-distance)))))


(defn take-entities-snapshot [g pid eids-on-client]
  (let [;; TODO: send only coords of entities, compare with prev packet
        ;; entities (for [[eid es] (:entities g)] {:xy (:xy es)})
        ps (get-in g [:entities pid])
        all-entitites (:entities g)
        entities (into {} (filter (fn [[_ e]] (user-can-view-entity ps e)) all-entitites))
        eids (keys entities)
        new-eids (remove eids-on-client eids)
        upd-eids (filter eids-on-client eids)]
    {:upd (map (comp entitiy-upd-obj entities) upd-eids)
     :add (map entities new-eids)
     :rem (remove (set eids) eids-on-client)
     }))


(defn move-entity
  [eid dx dy]
  (update-world!
   [g]
   (if-let [es (-> g :entities (get eid))]
     (let [[x y] (:xy es)
           x' (+ x (or dx 0))
           y' (+ y (or dy 0))]
       (assoc-in g [:entities eid :xy] [x' y']))
     g)))


(defn maybe-player-spawn-bullet [w pid user-input]
  (if-not (:fire? user-input)
    w
    (let [ps (get-in w [:entities pid])
          t1 (:at w)
          lbt (:last-bullet-at ps)]
      (if (and lbt (> lbt (- t1 bullet-cooldown)))
        w
        (let [{:keys [xy vxy angle]} ps
              b-vxy (v+ vxy (vas angle bullet-start-velocity))]
          (-> w
              (assoc-in [:entities pid :last-bullet-at] t1)
              (add-new-entity
               (assoc bullet-proto
                 :type :bullet
                 :pxy nil
                 :vxy b-vxy
                 :xy xy
                 :angle angle
                 :bullet-owner pid
                 :drop-at (+ (:at w) bullet-lifetime)
                 ))))))))


(defn change-user-engine-force [w pid user-input]
  [w]
  (let [a (:angle user-input 0)
        ed (:engine-dir user-input)
        ef (case ed -1 (- engine-reverse-force) 1 engine-forward-force 0)
        fxy (if (zero? ef) [0 0] (vas a ef))]
    (-> w
        (assoc-in [:entities pid :fxy] fxy)
        (assoc-in [:entities pid :angle] a))))


(defn game-process-user-input [pid user-input]
  (world->!
   (change-user-engine-force pid user-input)
   (maybe-player-spawn-bullet pid user-input)))


;; --

(defn search-new-player-pos [w pid]
  [(rand-int 2000) (rand-int 2000)])


(defn some-entity-collide-with [w es]
  (some #(when (is-colliding? es %) %) (vals (:entities w))))


(defn remove-entity [w eid]
  (update-in w [:entities] dissoc eid))


(defn inc-player-score [w pid]
  (update-in w [:entities pid :score] (fnil inc 0)))


(defn handle-entity-collision [w es es2]
  (cond
   (= (:type es) :player) (let [w1 (if (= (:type es2) :bullet)
                                     (inc-player-score w (:bullet-owner es2))
                                     w)
                                w2 (kill-player w1 (:eid es))]
                            w2)
   (:killable? es) (-> w (remove-entity (:eid es)))
   :else w))


(defn maybe-collide-entity-with-something [w es]
  (if-let [e2 (some-entity-collide-with w es)]
    (handle-entity-collision w es e2)
    w))


(defn kill-player [w pid]
  (let [xy' (search-new-player-pos w pid)]
    (-> w
        (update-in [:entities pid :deaths] (fnil inc 0))
        (update-in [:entities pid] merge {:xy xy' :pxy nil :vxy [0 0]}))))


(defn entity-out-of-gamefield? [w es]
  (let [[x y] (:xy es)]
    (or
     (< x (- world-width))
     (> x world-width)
     (< y (- world-height))
     (> y world-height))))


(defn player-and-ist-bullet [e1 e2]
  (and (= (:eid e1) (:bullet-owner e2))))

(defn is-colliding? [e1 e2]
  (cond
   (or (player-and-ist-bullet e1 e2) (player-and-ist-bullet e2 e1)) false
   :else
   (when (not= (:eid e1) (:eid e2))
     (let [r1 (:radius e1)
           r2 (:radius e2)
           xy1 (:xy e1)
           xy2 (:xy e2)]
       (and xy1 xy2 r1 r2 (< (distance xy1 xy2) (+ r1 r2)))))))

