(ns pintoid.client.ceh
  (:require
   [clojure.set :refer [union]])
  (:require-macros
   [taoensso.timbre :as timbre]))


(defrecord -World [time selfid entities])

;; == Public API

(defn empty-world []
  (new -World nil nil {}))


(defn entity [w eid]
  (get (.-entities w) eid))


(defn all-entities [w]
  (vals (.-entities w)))


(defn world-time [w]
  (.-time w))

(declare apply-world-patch)

(defn player-entity [w]
  (entity w (.-selfid w)))


(defn changed-eids [wpatch component]
  (eduction (map #(nth % 0))
            (get-in wpatch [:ecs component])))


;; == Impl

(defn- removed-entity? [e]
  (or
   (nil? e)
   (every? (fn [[c v]] (or (= :eid c) (nil? v))) e)))


(defn- merge-entities-map-and-comps [entities cid-eid-c]
  (persistent!
   (reduce
    (fn [es [cid cs]]
      (reduce
       (fn [es [eid c]]
         (let [ent (or (get es eid) {:eid eid})
               ent' (if (nil? c) (dissoc ent cid) (assoc ent cid c))]
           (if (removed-entity? ent')
             (dissoc! es eid)
             (assoc! es eid ent'))))
       es
       cs))
    (transient entities)
    cid-eid-c)))


(defn- build-eid-component-patch [es1 es2]
  (-> []
      (into (comp (remove es2) (map (fn [eid] [eid nil]))) (keys es1))
      (into (comp (remove es1) (map (fn [eid] [eid eid]))) (keys es2))))


(defn apply-world-patch [w wpatch]
  (let [es1 (.-entities w)
        es2 (merge-entities-map-and-comps es1 (:ecs wpatch))]
    [(-World.
      (or (:game-time wpatch) (world-time w))
      (or (:self wpatch) (.-player-eid w))
      es2)
     (assoc
      wpatch :eid
      (build-eid-component-patch es1 es2))]))
