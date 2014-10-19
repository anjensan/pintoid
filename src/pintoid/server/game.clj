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


(defn fix-world-state []
  @world)


(defn current-time []
  (System/currentTimeMillis))


(defn search-new-player-pos []
  [(rand-int 1000) (rand-int 1000)])


(defn random-player-color []
  (let [rcp #(format "%02X" (+ 10 (rand-int 150)))]
    (str "#" (rcp) (rcp) (rcp))))


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
(declare kill-outdated-entities)

(defn run-world-simulation-tick []
  (update-world! [w]
    (let [t1 (:at w)
          t2 (current-time)]
      (-> w
          (kill-outdated-entities t2)
          (update-entities-physics t1 t2)
          (assoc :at t2)
          ))))


(defn kill-outdated-entities [w t2]
  (let [ess (:entities w)
        ess' (into
              {}
              (remove
               (fn [[eid es]] (when-let [t (:drop-at es)] (<= t t2)))
               ess))]
    (assoc w :entities ess')))


(defn update-entity-physics-position [entity phobjs t1 t2]
  (if-not (:phys-move entity)
    entity
    (let [vxy (:vxy entity [0 0])
          xy (:xy entity)
          pxy (or (:pxy entity) (v+ xy (vs* vxy (- t1 t2))))
          m (:mass entity 1)
          fc (reduce v+ (:fxy entity [0 0])
                     (map #(calc-gravity-force m (:mass %) xy (:xy %)) phobjs))
          dt (- t2 t1)
          dxy (hardlimit-force-2d (vs* fc (/ 1 m)))
          xy' (integrate-verle-2d pxy xy dxy dt)
          vxy' (vs* (v- xy' xy) (/ 1 (- t2 t1)))]
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
  (let [xy (search-new-player-pos)
        ps (assoc player-proto :eid eid :xy xy)]
    (world->!
     (add-new-entity ps))
    ps))


(defn take-game-snapshot [g eid]
  {:player-eid eid
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
        entities all-entitites
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
              b-vxy (v+ vxy (vas angle bullet-start-velocity))
              b-xy (v+ xy (vs* b-vxy bullet-ahead-time))]
          (-> w
              (assoc-in [:entities pid :last-bullet-at] t1)
              (add-new-entity
               (assoc bullet-proto
                 :type :bullet
                 :pxy nil
                 :vxy b-vxy
                 :xy b-xy
                 :angle angle
                 :drop-at (+ (:at w) bullet-lifetime)
                 ))))))))


(defn change-user-engine-force [w pid user-input]
  [w]
  (let [a (:angle user-input 0)
        ed (:engine-dir user-input)
        ef (case ed -1 (- engine-reverse-force) 1 engine-forward-force 0)
        fxy (if (zero? ef) [0 0] (vas ef a))]
    (-> w
        (assoc-in [:entities pid :fxy] fxy)
        (assoc-in [:entities pid :angle] a))))


(defn game-process-user-input [pid user-input]
  (world->!
   (change-user-engine-force pid user-input)
   (maybe-player-spawn-bullet pid user-input)))

