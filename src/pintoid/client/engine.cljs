(ns pintoid.client.engine
  (:use [pintoid.client.ceh :only
         [entity
          all-entities
          world-time
          changed-eids
          empty-world
          apply-world-patch
          player-entity]]
        [clojure.walk :only
         [keywordize-keys]])
  (:require
   [pintoid.client.asset :as as]
   [pintoid.client.graphics.animation :as a]
   [pintoid.client.graphics.animloop :as al]
   [pintoid.client.graphics.layer :as gl]
   [pintoid.client.graphics.core :as g]
   )
  (:require-macros
   [taoensso.timbre :as timbre]
   [pintoid.client.macros :refer [foreach!]]))


;; world state
(def world (atom (empty-world)))
(def used-assets-by-sprite (atom {}))

(defmulti add-entity-sprite :type)
(defmulti remove-entity-sprite :type)


(defmethod add-entity-sprite :default [entity]
  (let [s (:sprite entity)
        eid (:eid entity)]
    (if (map? s)
      (foreach! [[k v] s] (g/new-sprite eid k v entity))
      (g/new-sprite eid nil s entity))))

(defmethod remove-entity-sprite :default [entity]
  (g/remove-sprites-by-eid (:eid entity)))


(defmulti select-entity-sprites
  (fn [purpose entity] [purpose (:type entity)]))

(defmethod select-entity-sprites :default [_ e]
  (g/find-sprites-by-eid (:eid e)))

(defn- foreach-entity-sprite [action entity f]
  (run! #(when % (f %)) (select-entity-sprites action entity)))

(defn handle-addrem-assets [w1 w2 wpatch]
  (when-let [eids (seq (sort (changed-eids w1 wpatch :asset)))]
    (let [ass (into (sorted-map) (map (fn [e] [e (:asset (entity w2 e))])) eids)
          updated-assets (as/add-assets ass)]
      (foreach! [[eid deps] @used-assets-by-sprite]
        (when (some updated-assets deps)
          (let [e1 (entity w1 eid)
                e2 (entity w2 eid)]
            (al/action!
              (world-time w2)
              (fn []
                (timbre/debug "Recreate sprite" eid)
                (remove-entity-sprite e1)
                (let [[_ deps] (as/track-used-assets add-entity-sprite e2)]
                  (swap! used-assets-by-sprite assoc eid deps)))))))
      (timbre/info "Assets reloaded"))))


(defn handle-sprites-movement [w1 w2 wpatch]
  (let [t1 (world-time w1)
        t2 (world-time w2)]
    (foreach! [eid (changed-eids w1 wpatch :position)]
      (let [e1 (entity w1 eid)
            e2 (entity w2 eid)
            xy1 (:position e1)
            xy2 (:position e2)
            tts1 (:position-tts e1)
            tts2 (:position-tts e2)]
        (when (and xy2 (not= xy1 xy2))
          (al/action!
           t1
           (fn []
             (foreach-entity-sprite
              :move e2
              (fn [obj]
                (if (and xy1 (= tts1 tts2))
                  (a/linear-move obj t1 t2 xy1 xy2)
                  (a/instant-move obj t2 xy2)
                  ))))))))))


(defn handle-sprites-rotation [w1 w2 wpatch]
  (let [t1 (world-time w1)
        t2 (world-time w2)]
    (foreach! [eid (changed-eids w1 wpatch :angle)]
      (let [e1 (entity w1 eid)
            e2 (entity w2 eid)
            a1 (:angle e1)
            a2 (:angle e2)]
        (when (and a2 (not= a1 a2))
          (al/action!
           t1
           (fn []
             (foreach-entity-sprite
              :rotate e2
              (fn [obj]
                (if a1
                  (a/linear-rotate obj t1 t2 a1 a2)
                  (a/instant-rotate obj t2 a2)))))))))))


(defn handle-player-camera-pos [w1 w2 wpatch]
  (let [t1 (world-time w1)
        t2 (world-time w2)
        p1 (player-entity w1)
        p2 (player-entity w2)
        [x1 y1] (:position p1)
        [x2 y2] (:position p2)]
    (a/linear-animate
     t1 t2 1 0
     (fn [a]
       (let [b (- 1 a)]
         (gl/set-viewport!
          (+ (* a x1) (* b x2))
          (+ (* a y1) (* b y2)) 1)
         true)))))


(defn format-player-score [entity]
  (str (:score entity)))


(defn handle-players-score [w1 w2 wpatch]
  (foreach! [eid (changed-eids w1 wpatch :score)]
    (when-let [s (g/get-sprite eid :score-label)]
      (let [t (format-player-score (entity w2 eid))]
        (al/action!
          (world-time w2)
          #(set! (.-text s) t))))))


(defn handle-add-sprites [w1 w2 wpatch]
  (foreach! [eid (changed-eids w1 wpatch :sprite)]
    (let [e (entity w2 eid)]
      (when (:sprite e)
        (al/action!
         (world-time w2)
         (fn []
           (timbre/trace "Add sprite " e)
           (let [[_ deps] (as/track-used-assets add-entity-sprite e)]
             (swap! used-assets-by-sprite assoc eid deps))))))))


(defn handle-remove-sprites [w1 w2 wpatch]
  (foreach! [eid (changed-eids w1 wpatch :sprite)]
    (when-not (:sprite (entity w2 eid))
      (al/action!
       (world-time w2)
       (fn []
         (swap! used-assets-by-sprite dissoc eid)
         (timbre/trace "Remove sprite " (entity w1 eid))
         (remove-entity-sprite (entity w1 eid)))))))


(defmethod add-entity-sprite :player [entity]
  (let [eid (:eid entity)
        sprite (if (:self-player entity)
                 'pintoid.server.data.assets/racket-red
                 'pintoid.server.data.assets/racket-blue)]
    (g/new-sprite eid sprite entity)
    (g/new-sprite eid :score-label
                  'pintoid.server.data.assets/player-score
                  (assoc entity :text (format-player-score entity))
  )))

(defmethod select-entity-sprites [:rotate :player] [_ entity]
  [(g/get-sprite (:eid entity))])


(defn update-world-snapshot! [wpatch]
  (let [w1 @world, [w2 wpatch'] (apply-world-patch w1 wpatch)]
    (handle-remove-sprites w1 w2 wpatch)
    (handle-addrem-assets w1 w2 wpatch)
    (handle-add-sprites w1 w2 wpatch)
    (handle-sprites-movement w1 w2 wpatch)
    (handle-sprites-rotation w1 w2 wpatch)
    (handle-players-score w1 w2 wpatch)
    (handle-player-camera-pos w1 w2 wpatch)
    (reset! world w2)))
