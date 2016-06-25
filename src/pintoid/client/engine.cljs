(ns pintoid.client.engine
  (:use [pintoid.client.ceh :only
         [entity
          world-time
          changed-eids
          empty-world
          apply-world-patch
          player-entity]]
        [clojure.walk :only
         [keywordize-keys]])
  (:require [pintoid.client.animation :as a]
            [pintoid.client.graphics :as g])
  (:require-macros
   [pintoid.client.utils :refer [log foreach!]]))


;; map: entity-id -> pixi-obj (root)
(def eid-pixiobj (atom {}))

;; world state
(def world (atom (empty-world)))


(declare handle-addrem-sprite-entities)
(declare handle-sprites-movement!)
(declare handle-sprites-rotation!)
(declare handle-player-state!)


(defn update-world-snapshot! [wpatch]
  (log :debug "update world snapshot" wpatch)
  (let [w1 @world
        w2 (swap! world apply-world-patch wpatch)]
    (handle-addrem-sprite-entities w1 w2 wpatch)
    (handle-sprites-movement! w1 w2 wpatch)
    (handle-sprites-rotation! w1 w2 wpatch)
    (handle-player-state! w1 w2 wpatch)
    ))


(defn resolve-entity-object [eid]
  ;; lazily create pixi obj?
  (get @eid-pixiobj eid))


(defn handle-sprites-movement! [w1 w2 wpatch]
  (let [t1 (world-time w1)
        t2 (world-time w2)]
    (foreach! [eid (changed-eids wpatch :xy)]
      (when-let [obj (resolve-entity-object eid)]
        (let [e1 (entity w1 eid)
              e2 (entity w2 eid)
              xy1 (:xy e1)
              xy2 (:xy e2)]
          (when (and xy2 (not= xy1 xy2))
            (if xy1
              (a/linear-move! obj t1 t2 xy1 xy2)
              (a/instant-move! obj t1 t2 xy2))))))))


(defn handle-sprites-rotation! [w1 w2 wpatch]
  (let [t1 (world-time w1)
        t2 (world-time w2)]
    (foreach! [eid (changed-eids wpatch :angle)]
      (when-let [obj (resolve-entity-object eid)]
        (let [e1 (entity w1 eid)
              e2 (entity w2 eid)
              a1 (:angle e1)
              a2 (:angle e2)]
          (when (and a2 (not= a1 a2))
            (if a1
              (a/linear-rotate! obj t1 t2 a1 a2)
              (a/instant-rotate! obj t1 t2 a2))))))))


(defn handle-player-state! [w1 w2 wpatch]
  (let [t2 (world-time w2)
        t1 (world-time w1)
        p1 (player-entity w1)
        p2 (player-entity w2)
        pxy1 (:xy p1)
        pxy2 (:xy p2)
        deaths (:deaths p2)
        score (:score p2)]
    (g/move-player-camera! t1 t2 pxy1 pxy2)
    (a/add-action!
     t2
     (fn []
       (g/update-player-score! score)
       (g/update-player-death! deaths)))))


(defn handle-addrem-sprite-entities [w1 w2 wpatch]
  (foreach! [eid (changed-eids wpatch :eid)]
    (let [entity (entity w2 eid)]
      (if (:eid entity)
        ;; TODO: split into 2 handlers.
        ;; add new entity
        (when-let [obj (g/create-entity-pixi-object entity)]
          (swap! eid-pixiobj assoc eid obj)
          (when (#{"star" "ast"} (:type entity) )
            (a/infinite-linear-rotate! obj 1e-3))
          (when (#{"black"} (:type entity) )
            (a/infinite-linear-rotate! obj 1)))
        ;; remove entity
        (when-let [obj (resolve-entity-object eid)]
          (a/add-action!
           (world-time w2)
           (fn []
             (g/delete-entity-pixi-object obj)
             (swap! eid-pixiobj dissoc eid))))))))
