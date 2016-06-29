(ns pintoid.client.ceh
  (:use
   [clojure.set :only [union]])
  (:require-macros
   [pintoid.client.macros :refer [log]]))


(defrecord -World [time selfid entities])

;; == Public API


(defn empty-world []
  (new -World nil nil {}))


(defn entity [w eid]
  (get (.-entities w) eid))


(defn world-time [w]
  (.-time w))


(defn player-entity [w]
  (entity w (.-selfid w)))


(declare merge-entities-map-and-comps)


(defn apply-world-patch [w wpatch]
  (-World.
   (or (:time wpatch) (world-time w))
   (or (:self wpatch) (.-player-eid w))
   (merge-entities-map-and-comps (.-entities w) (:ecs wpatch))
   ))


(defn changed-eids [wpatch component]
  (eduction (map #(nth % 0))
            (get-in wpatch [:ecs component])))


(defn- merge-entities-map-and-comps [entities cid-eid-c]
  (persistent!
    (reduce
     (fn [es [cid cs]]
       (reduce
        (fn [es [eid c]]
          (let [ent (or (get es eid) {:eid eid})
                ent' (if (nil? c) (dissoc ent cid) (assoc ent cid c))]
            ;; TODO: Cleanup removed entities & modify wpatch/changed-eids (add removed eids)
            (assoc! es eid ent')))
        es
        cs))
     (transient entities)
     cid-eid-c)))
