(ns pintoid.server.game
  (:use [pintoid.server utils physics])
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

(defn add-clojure-entity-test! [n]
  (dotimes [_ n]
    (let [eid (next-eid)]
      (world->!
       (assoc :at (current-time))
       (add-new-entity
        {:eid eid
         :xy [(rand-int 500) (rand-int 500)]
         :mass 100
         :phys true
         })))))

(defn init-world-state []
  (world->!
   (assoc :at (current-time))))
;  (add-clojure-entity-test! 5))


(declare update-entities-physics)

(defn run-world-simulation-tick []
  (update-world! [w]
    (let [t1 (:at w)
          t2 (current-time)]
      (-> w
          (assoc :at t2)
          (update-entities-physics t1 t2)
          ))))


(defn update-entity-physics-position [entity phobjs t1 t2]
  (if-not (:phys-move entity)
    entity
    (let [xy (:xy entity)
          pxy (:pxy entity xy)
          m (:mass entity 1)
          fc (reduce v+ (:fxy entity [0 0])
                     (map #(calc-gravity-force m (:mass %) xy (:xy %)) phobjs))
          dt (- t2 t1)
          dxy (hardlimit-force-2d (vs* fc (/ 1 m)))
          xy' (integrate-verle-2d pxy xy dxy dt)]
      (assoc entity :pxy xy :xy xy'))))


(defn update-entities-physics [w t1 t2]
  (let [es (:entities w)
        phobjs (filter :phys (vals es))
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
        color (random-player-color)
        ps {:type :player
            :eid eid
            :xy xy
            :fxy [0 0]
            :color color
            :texture :clojure
            :phys-move true
            :mass 10
            }]
    (world->!
     (add-new-entity ps))
    ps))


(defn take-game-snapshot [g eid]
  {:player-eid eid})


(defn entitiy-upd-obj [entity]
  {:eid (:eid entity)
   :xy (:xy entity)
   :angle (:angle entity)})


(defn take-entities-snapshot [g pid eids-on-client]
  (let [;; TODO: send only coords of entities, compare with prev packet
        ;; entities (for [[eid es] (:entities g)] {:xy (:xy es)})
        entities (:entities g)
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


(defn game-process-user-input [pid m]
  (update-world!
   [w]
   (let [a (:angle m 0)
         ego? (:engine-on? m)
         ef (if ego? engine-force 0)
         fxy (if (zero? ef) [0 0] (vs* [(Math/cos a) (Math/sin a)] ef))]
     (assoc-in w [:entities pid :fxy] fxy))))

