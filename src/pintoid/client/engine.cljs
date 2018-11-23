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
   [pintoid.client.sound :as snd]
   [pintoid.client.animloop :as al]
   [pintoid.client.animation :as a]
   [pintoid.client.layer :as gl]
   [pintoid.client.graphics :as g]
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
  (timbre/debug "Add entity sprite" entity)
  (let [s (:sprite entity)
        eid (:eid entity)]
    (if (map? s)
      (foreach! [[k v] s] (g/new-sprite eid k v entity))
      (g/new-sprite eid nil s entity))))

(defmethod remove-entity-sprite :default [entity]
  (timbre/debug "Remove entity sprite" entity)
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
      (timbre/debug "Assets loaded"))))

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

(defn handle-player-sound-pos [w1 w2 wpatch]
  (let [t2 (world-time w2)
        p2 (player-entity w2)
        xy (:position p2)]
    (al/action! t2 #(snd/set-listener-pos xy))))

(defn format-player-score [entity]
  (let [{:keys [score nick]} (get entity :player)]
    (str nick " " score)))

(defn handle-players-score [w1 w2 wpatch]
  (foreach! [eid (changed-eids w1 wpatch :player)]
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

(defn handle-addrem-sounds [w1 w2 wpatch]
  (foreach! [eid (changed-eids w1 wpatch :sound)]
    (let [t2 (world-time w2)
          e1 (entity w1 eid)
          e2 (entity w2 eid)
          s1 (:sound e1 {})
          s2 (:sound e2 {})]
      (doseq [[k v2] s2]
        (when-not (= (get s1 k) v2)
          (al/action! t2 #(snd/play-sound eid k v2))))
      (doseq [[k v2] s1]
        (when-not (contains? s2 k)
          (al/action! t2 #(snd/stop-sound eid k))))
      )))

(defn handle-move-sound-pos [w1 w2 wpatch]
  (let [t2 (world-time w2)]
    (foreach! [eid (changed-eids w1 wpatch :position)]
      (let [e2 (entity w2 eid)]
        (when (:sound e2)
          (al/action! t2 #(snd/set-sound-pos eid (:position e2))))))))

(defmethod add-entity-sprite :player [entity]
  (timbre/debug "Add player entity sprite" entity)
  (let [eid (:eid entity)
        sprite (if (:self-player entity)
                 'pintoid.assets.sprites/racket-red
                 'pintoid.assets.sprites/racket-blue)]
    (g/new-sprite eid sprite entity)
    (g/new-sprite eid :score-label
                  'pintoid.assets.sprites/player-score
                  (assoc entity :text (format-player-score entity))
  )))

(defmethod select-entity-sprites [:rotate :player] [_ entity]
  [(g/get-sprite (:eid entity))])

(defn update-world-snapshot! [wpatch]
  (let [w1 @world, [w2 wpatch'] (apply-world-patch w1 wpatch)]
    (doseq [f [
               handle-remove-sprites
               handle-addrem-assets
               handle-add-sprites
               handle-sprites-movement
               handle-sprites-rotation
               handle-players-score
               handle-player-camera-pos
               handle-player-sound-pos
               handle-addrem-sounds
               handle-move-sound-pos
               ]]
      (f w1 w2 wpatch))
    (reset! world w2)))
