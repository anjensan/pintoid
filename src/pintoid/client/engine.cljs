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
      (foreach! [[k v] s] (g/new-sprite eid k v))
      (g/new-sprite eid nil s))))

(defmethod remove-entity-sprite :default [entity]
  (timbre/trace "Remove entity sprite" entity)
  (g/remove-sprites-by-eid (:eid entity)))

(defn- foreach-entity-sprite [entity f]
  (run! #(when % (f %)) (g/find-sprites-by-eid (:eid entity))))

(defn- do-add-entity-sprite [e]
  (timbre/trace "Add sprite " e)
  (let [eid (:eid e)
        xy2 (:position e)
        a2 (:angle e)
        [_ deps] (as/track-used-assets add-entity-sprite e)]
    (swap! used-assets-by-sprite assoc eid deps)
    (when xy2 (foreach-entity-sprite e #(a/instant-move % 0 xy2)))
    (when a2 (foreach-entity-sprite e #(a/instant-rotate % 0 a2)))))

(defn- do-remove-entity-sprite [e]
  (let [eid (:eid e)]
    (timbre/trace "Remove sprite " e)
    (swap! used-assets-by-sprite dissoc eid))
  (remove-entity-sprite e))

(defn- do-update-entity-sprites [e1 e2]
  (let [s1 (:sprites e1 {})
        s2 (:sprites e2 {})
        s1 (if (map? s1) s1 {nil s1})
        s2 (if (map? s2) s2 {nil s2})
        common (map key (filter #(= (val %) (get s1 (key %))) s2))
        s1 (apply dissoc s1 common)
        s2 (apply dissoc s2 common)]
    (do-remove-entity-sprite (assoc e1 :sprites s1))
    (do-add-entity-sprite (assoc e2 :sprites s2))))

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
                (do-remove-entity-sprite e1)
                (do-add-entity-sprite e2))))))
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
              e2
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
              e2
              (fn [obj]
                (if a1
                  (a/linear-rotate obj t1 t2 a1 a2)
                  (a/instant-rotate obj t2 a2)))))))))))

(defn handle-player-camera-pos [w1 w2 wpatch]
  (let [t1 (world-time w1)
        t2 (world-time w2)
        p1 (player-entity w1)
        p2 (player-entity w2)
        [x1 y1 s1] (:camera p1)
        [x2 y2 s2] (:camera p2)]
    (a/linear-animate
     t1 t2 1 0
     (fn [a]
       (let [b (- 1 a)]
         (gl/set-viewport!
          [(+ (* a x1) (* b x2))
           (+ (* a y1) (* b y2))]
          (+ (* a s1) (* b s2)))
         true)))))

(defn handle-player-sound-pos [w1 w2 wpatch]
  (let [t2 (world-time w2)
        p2 (player-entity w2)
        xy (:position p2)]
    (al/action! t2 #(snd/set-listener-pos xy))))

(defn handle-add-sprites [w1 w2 wpatch]
  (foreach! [eid (changed-eids w1 wpatch :sprite)]
    (let [e1 (entity w1 eid)
          e2 (entity w2 eid)
          t2 (world-time w2)]
      (when (and (not (:sprite e1)) (:sprite e2))
        (al/action! t2 #(do-add-entity-sprite e2))))))

(defn handle-remove-sprites [w1 w2 wpatch]
  (foreach! [eid (changed-eids w1 wpatch :sprite)]
    (when-not (:sprite (entity w2 eid))
      (al/action!
       (world-time w2)
       #(do-remove-entity-sprite (entity w1 eid))))))

(defn handle-update-sprites [w1 w2 wpatch]
  (foreach! [eid (changed-eids w1 wpatch :sprite)]
    (let [e1 (entity w1 eid)
          e2 (entity w2 eid)
          t2 (world-time w2)]
      (when (and e1 e2)
        (al/action! t2 #(do-update-entity-sprites e1 e2))))))

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

(declare handle-players-score)

(defn update-world-snapshot! [wpatch]
  (let [w1 @world, [w2 wpatch'] (apply-world-patch w1 wpatch)]
    (doseq [f [
               handle-remove-sprites
               handle-addrem-assets
               handle-update-sprites
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

;; TODO: move into separate ns.

(defn format-player-score [entity]
  (let [{:keys [score nick]} (get entity :player)]
    (str nick " " score)))

(defn handle-players-score [w1 w2 wpatch]
  (foreach! [eid (changed-eids w1 wpatch :player)]
    (when-let [s (g/get-sprite eid :score-label)]
      (let [t (format-player-score (entity w2 eid))]
        (al/action! (world-time w2) #(set! (.-text s) t))))))

(defmethod add-entity-sprite :player [entity]
  (timbre/debug "Add player entity sprite" entity)
  (add-entity-sprite (dissoc entity :type))
  (let [s (:sprite entity)
        eid (:eid entity)
        sprite (if (:self-player entity)
                 'pintoid.assets.sprites/racket-red
                 'pintoid.assets.sprites/racket-blue)]
    (timbre/trace "Recreate player sprite")
    (g/new-sprite eid nil sprite)
    (g/new-sprite eid :score-label
                  'pintoid.assets.sprites/player-score
                  {:text (format-player-score entity)})))
