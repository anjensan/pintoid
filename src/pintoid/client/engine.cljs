(ns pintoid.client.engine
  (:use [pintoid.client.animation :only
         [add-action!
          defer-action!
          linear-move!]]
        [pintoid.client.graphics :only
         [create-entity-pixi-object
          delete-entity-pixi-object
          move-player-camera!
          ]]
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
(declare drop-entity!)

;; ss - SnapShot -- JSON OBJECT!

(defn update-world-snapshot! [at game-upd entts]
  (log :debug "update world snapshot" at game-upd entts)
  (reset!
   world
   (-> @world
       (handle-rem-entities (aget entts "rem") at)
       (handle-add-entities (aget entts "add") at)
       (handle-upd-entities (aget entts "upd") at)
       (update-game game-upd)
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


(defn update-game [w gup]
  w)
  ;; (let [xy (js->clj (aget ss "player-xy"))]
  ;;   (-> w
  ;;       (update-in [:player :xy] xy)
  ;;       (add-action move-player-camera! xy))))


(defn- handle-add-entities [w es at]
  (log :debug "add" (count es) "entities")
  (let [add-ent-fn
        (fn [w ent-state-json]
          (let [ent (keywordize-keys (js->clj ent-state-json))
                eid (:eid ent)]
            (log :trace "add entity" eid ent)
            (-> w
                (update-in [:entities] assoc eid ent)
                (add-action add-entity! eid at ent))))]
    (reduce add-ent-fn w es)))


(defn- handle-rem-entities [w es at]
  :todo
  w)


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
                (add-action update-entity! eid old-state t1 new-state t2 ))))]
    (reduce add-ent-fn w es)))


(defn resolve-entity-object [eid]
  ;; lazily create pixi obj?
  (get @eid-pixiobj eid))


(defn drop-entity! [eid when]
  ;; delete animation?
  (let [obj (resolve-entity-object eid)]
    (add-action!
     when
     (fn []
       (delete-entity-pixi-object obj)
       (swap! eid-pixiobj dissoc eid)
       ))))


(defn add-entity! [eid when entity]
  (let [obj (create-entity-pixi-object entity)]
    (swap! eid-pixiobj assoc eid obj)))


(defn update-entity! [eid estate1 t1 estate2 t2]
  (let [old-xy (:xy estate1)
        new-xy (:xy estate2)]
    (when (not= old-xy new-xy)
      (let [obj (resolve-entity-object eid)]
        (linear-move! nil obj t1 t2 old-xy new-xy)))))
