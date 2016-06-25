(ns pintoid.client.engine
  (:use [pintoid.client.animation :only
         [add-action!
          defer-action!
          linear-move!
          linear-rotate!
          infinite-linear-rotate!]]
        [pintoid.client.graphics :only
         [create-entity-pixi-object
          delete-entity-pixi-object
          move-player-camera!
          update-pixi-score!
          update-pixi-death!]]
        [clojure.walk :only [keywordize-keys]])
  (:require-macros
   [pintoid.client.utils :refer [log]]))



(def empty-world
  {
   :at 0
   :self-eid nil
   :player {:xy [0 0]}
   :entities {}                         ; {entity-id -> entity}
   ;; eid - entity id, number
   :actions ()
   })

;; map: entity-id -> pixi-obj (root)
(def eid-pixiobj (atom {}))

;; world state
(def world (atom empty-world))

;; --

(declare update-world-snapshot!)

(declare handle-add-entities)
(declare handle-upd-entities)
(declare handle-rem-entities)
(declare update-game)

(declare add-action)
(declare run-actions!)

(declare update-entity!)
(declare add-entity!)
(declare remove-entity!)
(declare update-player-score!)

;; ss - SnapShot -- JSON OBJECT!

(defn update-world-snapshot! [at game-upd entts]
  (log :debug "update world snapshot" at game-upd entts)
  (reset!
   world
   (-> @world
       (handle-rem-entities (aget entts "rem") at)
       (handle-add-entities (aget entts "add") at)
       (handle-upd-entities (aget entts "upd") at)
       (update-game game-upd at)
       (assoc :at (long at))
       (run-actions!))))


(defn update-player-state! [ps]
  (log :debug "update player" ps)
  (swap! world
         (fn [w]
           (let [xy (:xy ps)
                 eid (:eid ps)]
             (-> w
                 (assoc-in [:player :xy] xy)
                 (assoc :self-eid eid))))))


(defn add-action
  ([w af] (let [a (:actions w)
                a' (conj a af)]
            (assoc w :actions a')))
  ([w af x] (add-action w #(af x)))
  ([w af x y] (add-action w #(af x y)))
  ([w af x y & z] (add-action w #(apply af x y z))))


(defn run-actions! [w]
  (let [as (:actions w)]
    (doseq [a as] (a))
    (assoc w :actions ())))


(defn update-game [w gup t2]
  (let [t1 (:at w)
        cur-pxy (get-in w [:player :xy])
        pxy (:player-xy gup)
        deaths (:deaths gup 0)
        score (:score gup 0)]
    (-> w
     (assoc-in [:player :xy] pxy)
     (add-action update-player-score! t2 deaths score)
     (add-action move-player-camera! t1 t2 cur-pxy pxy))))


(defn- handle-add-entities [w es at]
  (log :debug "add" (count es) "entities")
  (let [add-ent-fn
        (fn [w ent-state-json]
          (let [ent (keywordize-keys (js->clj ent-state-json))
                eid (:eid ent)
                entx (if (= eid (:self-eid w))
                       (assoc ent :type :self-player)
                       ent)
                ]
            (log :trace "add entity" eid ent)
            (-> w
                (update-in [:entities] assoc eid entx)
                (add-action add-entity! eid at entx))))]
    (reduce add-ent-fn w es)))


(defn- handle-rem-entities [w es at]
  (log :debug "rem" (count es) "entities")
  (let [rem-ent-fn
        (fn [w eid]
          (log :trace "add entity" eid)
          (-> w
              (update-in [:entities] dissoc eid)
              (add-action remove-entity! eid at
                          (get-in w [:entities eid]))))]
    (reduce rem-ent-fn w es)))

(defn merge-entity-upd [old-state patch]
  (merge old-state (keywordize-keys (js->clj patch))))


(defn- handle-upd-entities [w es at]
  (log :debug "upd" (count es) "entities")
  (let [t1 (:at w)
        t2 at
        add-ent-fn
        (fn [w ent-patch-json]
          (let [eid (aget ent-patch-json "eid")
                old-state (get-in w [:entities eid])
                new-state (merge-entity-upd old-state ent-patch-json)]
            (log :trace "upd entity" eid patch)
            (-> w
                (update-in [:entities] assoc eid new-state)
                (add-action update-entity! eid old-state t1 new-state t2))))]
    (reduce add-ent-fn w es)))


(defn resolve-entity-object [eid]
  ;; lazily create pixi obj?
  (get @eid-pixiobj eid))


(defn remove-entity! [eid t2 estate]
  ;; delete animation?
  (when-let [obj (resolve-entity-object eid)]
    (add-action!
     t2
     (fn []
       (delete-entity-pixi-object obj)
       (swap! eid-pixiobj dissoc eid)
       ))))


(defn add-entity! [eid _ entity]
  (when-let [obj (create-entity-pixi-object entity)]
    ; (println ".-type"  (:type entity))
    (when (#{"star" "ast"} (:type entity) ) 
      (infinite-linear-rotate! nil obj 1e-3))

    (when (#{"black"} (:type entity) ) 
      (infinite-linear-rotate! nil obj 1))
    (swap! eid-pixiobj assoc eid obj)))


(defn update-entity! [eid estate1 t1 estate2 t2]
  (let [old-xy (:xy estate1)
        new-xy (:xy estate2)
        angle1 (:angle estate1)
        angle2 (:angle estate2)
        ]
    (when (not= old-xy new-xy)
      (when-let [obj (resolve-entity-object eid)]
        (linear-move! nil obj t1 t2 old-xy new-xy)))

    (when (not= angle1 angle2)
      (when-let [obj (resolve-entity-object eid)]
        (linear-rotate! nil obj t1 t2 angle1 angle2)))))

(defn update-player-score! [t1 deaths score]
  (add-action!
   t1
   (fn []
     (update-pixi-score! score)
     (update-pixi-death! deaths))))
