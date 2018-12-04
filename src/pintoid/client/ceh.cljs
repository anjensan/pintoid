(ns pintoid.client.ceh
  (:require
   [clojure.set :refer [union]])
  (:require-macros
   [taoensso.timbre :as timbre]))

(defrecord -World [time selfid entities])

(defn empty-world []
  (new -World nil nil {}))

(defn entity [w eid]
  (get (.-entities w) eid))

(defn all-entities [w]
  (vals (.-entities w)))

(defn world-time [w]
  (.-time w))

(defn player-entity [w]
  (entity w (.-selfid w)))

(defn changed-eids [old-world wpatch component]
  (eduction (comp
             (filter (fn [[e c]] (not= (get (entity old-world e) component) c)))
             (map #(nth % 0)))
            (get-in wpatch [:ecs component])))

(defn- merge-entities-map-and-comps [entities cid-eid-c]
  (persistent!
   (reduce
    (fn [es [cid cs]]
      (reduce
       (fn [es [eid c]]
         (let [ent (or (get es eid) {:eid eid})
               ent' (if (nil? c)
                      (dissoc ent cid)
                      (assoc ent cid c))]
           (if (<= (count ent') 1)
             (dissoc! es eid)  ;; eid' - nil or just {:eid ..}
             (assoc! es eid ent'))))
       es
       cs))
    (transient entities)
    cid-eid-c)))

(defn apply-world-patch [w wpatch]
  (let [es1 (.-entities w)
        es2 (merge-entities-map-and-comps es1 (:ecs wpatch))]
    (-World.
      (or (:game-time wpatch) (world-time w))
      (or (:self wpatch) (.-player-eid w))
      es2)))
