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
  (:require
   [pintoid.client.graphics.animation :as a]
   [pintoid.client.graphics.animloop :as al]
   [pintoid.client.graphics.sprite :as gs]
   [pintoid.client.graphics.core :as g])
  (:require-macros
   [pintoid.client.macros :refer [log foreach!]]))


;; map: entity-id -> pixi-obj (root)
(def eid-pixiobj (atom {}))

;; world state
(def world (atom (empty-world)))


;; TODO: Merge textures / sprites into packs/lists.

(defn handle-sprites-prototypes! [w1 w2 wpatch]
  (foreach! [eid (changed-eids wpatch :sprite-proto)]
    (foreach! [[id proto] (:sprite-proto (entity w2 eid))]
      (gs/add-prototype id proto))))


(defn handle-textures-info! [w1 w2 wpatch]
  (foreach! [eid (changed-eids wpatch :texture-info)]
    (foreach! [[id tinfo] (:texture-info (entity w2 eid))]
      (gs/add-texture id tinfo))))


(defn handle-sprites-movement! [w1 w2 wpatch]
  (let [t1 (world-time w1)
        t2 (world-time w2)]
    (foreach! [eid (changed-eids wpatch :position)]
      (when-let [obj (g/get-sprite eid)]
        (let [e1 (entity w1 eid)
              e2 (entity w2 eid)
              xy1 (:position e1)
              xy2 (:position e2)]
          (when (and xy2 (not= xy1 xy2))
            (if xy1
              (a/linear-move obj t1 t2 xy1 xy2)
              (a/instant-move obj t1 t2 xy2))))))))


(defn handle-sprites-rotation! [w1 w2 wpatch]
  (let [t1 (world-time w1)
        t2 (world-time w2)]
    (foreach! [eid (changed-eids wpatch :angle)]
      (when-let [obj (g/get-sprite eid)]
        (let [e1 (entity w1 eid)
              e2 (entity w2 eid)
              a1 (:angle e1)
              a2 (:angle e2)]
          (when (and a2 (not= a1 a2))
            (if a1
              (a/linear-rotate obj t1 t2 a1 a2)
              (a/instant-rotate obj t1 t2 a2))))))))


(defn handle-player-state! [w1 w2 wpatch]
  (let [t2 (world-time w2)
        t1 (world-time w1)
        p1 (player-entity w1)
        p2 (player-entity w2)
        pxy1 (:position p1)
        pxy2 (:position p2)
        deaths (:deaths p2)
        score (:score p2)]
    (g/move-player-camera! t1 t2 pxy1 pxy2)
    (al/add-action!
     t2
     (fn []
       (g/update-player-score! score)
       (g/update-player-death! deaths)))))


(defmulti add-entity-sprite (comp keyword :type))

(defn handle-addrem-sprite-entities [w1 w2 wpatch]
  (foreach! [eid (changed-eids wpatch :sprite)]
    (let [entity (entity w2 eid)]
      (if (:sprite entity)
        ;; TODO: split into 2 handlers.
        ;; add new entity
        ;; TODO: Also we need to specify correct :position etc.
        (al/add-action!
         (world-time w2)
         (fn [] (add-entity-sprite entity)))
        ;; remove entity
        (when-let [obj (g/get-sprite eid)]
          (al/add-action!
           (world-time w2)
           (fn []
             (g/remove-sprite eid))))))))


(defmethod add-entity-sprite :default [entity]
  (let [obj (g/new-sprite entity)]
    ;; TODO: Replace with loop animations / effects.
    (when (#{"star" "ast"} (:type entity) )
      (a/infinite-linear-rotate obj 1e-3))
    (when (#{"black"} (:type entity) )
      (a/infinite-linear-rotate obj 1))))


(defmethod add-entity-sprite :player [entity]
  (g/new-sprite
   (assoc entity
          :sprite (if (:self-player entity) :racket-red :racket-blue))))


(defn update-world-snapshot! [wpatch]
  (log :debug "update world snapshot" wpatch)
  (let [w1 @world
        w2 (swap! world apply-world-patch wpatch)]
    (handle-textures-info! w1 w2 wpatch)
    (handle-sprites-prototypes! w1 w2 wpatch)
    (handle-addrem-sprite-entities w1 w2 wpatch)
    (handle-sprites-movement! w1 w2 wpatch)
    (handle-sprites-rotation! w1 w2 wpatch)
    (handle-player-state! w1 w2 wpatch)
    ))
