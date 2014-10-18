(ns pintoid.server.game
  (:require
   [clojure.core.async :refer
    [<! >! put! close! go-loop go timeout]]))

;; API

(declare fix-world-state)
(declare create-entities-snapshot)

;; -- consts

(def eid-counter (atom 0))

(def world
  (agent
   {
    :at 0
    :players {}                         ; pid -> eid
    :entities {}                        ; map of active entities
    }))


(declare move-entity)
(declare next-eid)


(defn fix-world-state []
  @world)


(defn current-time []
  (System/currentTimeMillis))


(defn next-eid []
  (swap! eid-counter inc))


(defn search-new-player-pos []
  [(rand-int 1000) (rand-int 1000)])


(defn random-player-color []
  (let [rcp #(format "%02X" (+ 10 (rand-int 150)))]
    (str "#" (rcp) (rcp) (rcp))))


(defmacro world-let!
  [[s] & body]
  `(send world (fn [~s] ~@body)))


(defmacro world->!
  [& body]
  `(world-let! [w#] (-> w# ~@body)))


(defn add-new-player
  [pid]
  (let [eid (next-eid)
        xy (search-new-player-pos)
        color (random-player-color)
        ps {:type :player
            :eid eid
            :pid pid
            :xy xy
            :dxy [0 0]
            :color color}]
    (world->!
     (assoc-in [:players pid] eid)
     (assoc-in [:entities eid] ps))
    ps))


(defn remove-player [pid]
  (world-let! [w]
   (let [entities (:entities w)
         players (:players w)
         eid (players pid)]
     (-> w
         (assoc :entities (dissoc entities eid))
         (assoc :players (dissoc players pid))))))


(defn take-game-snapshot [g pid]
  {:player (get-in g [:players :pid])
   })


(defn take-entities-snapshot [g pid]
  (let [;; TODO: send only coords of entities, compare with prev packet
        ;; entities (for [[eid es] (:entities g)] {:xy (:xy es)})
        entities (:entities g)]
    {:upd entities}))


(defn move-entity
  [eid dx dy]
  (world-let!
   [g]
   (if-let [es (-> g :entities (get eid))]
     (let [[x y] (:xy es)
           x' (+ x (or dx 0))
           y' (+ y (or dy 0))]
       (assoc-in g [:entities eid :xy] [x' y']))
     g)))
