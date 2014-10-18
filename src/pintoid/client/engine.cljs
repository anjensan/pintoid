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
        [clojure.walk :only [keywordize-keys]]))


(def empty-world
  {
   :at 0
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

(declare handle-new-entities)
(declare handle-upd-entities)
(declare handle-rem-entities)
(declare update-player)
(declare update-time)

(declare add-action)
(declare run-actions!)

(declare update-entity!)
(declare add-entity!)
(declare drop-entity!)

;; ss - SnapShot -- JSON OBJECT!

(defn update-world-snapshot! [ss-json-str]
  (let [ss (js/JSON.parse ss-json-str)]
    (reset!
     world
     (-> @world
         (handle-rem-entities ss)
         (handle-new-entities ss)
         (handle-upd-entities ss)
         (update-player ss)
         (update-time ss)
         (run-actions!)))))


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


(defn update-time [w ss]
  (assoc w :at (long (aget ss "at"))))


(defn update-player [w ss]
  (let [xy (js->clj (aget ss "player-xy"))]
    (-> w
        (update-in [:player :xy] xy)
        (add-action move-player-camera! xy))))


(defn- handle-new-entities [w ss]
  :todo
  w)


(defn- handle-rem-entities [w ss]
  :todo
  w)


(defn- handle-upd-entities [w ss]
  :todo
  w)


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


(defn update-entity! [eid estate1 when1 estate2 when2]
  (let [old-xy (:xy estate1)
        new-xy (:xy estate2)]
    (when (not= old-xy new-xy)
      (let [obj (resolve-entity-object eid)]
        (linear-move! nil obj when1 when2 old-xy new-xy)))))
