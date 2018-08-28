(ns pintoid.server.game.kill
  (:use
   [pintoid.server.ecs core]
   [pintoid.server.game player])
  (:require
   [pintoid.server.vec2 :as v2]
   [taoensso.timbre :as timbre]
   [pintoid.server.game-maps :as gm]))


(defn- kill-player [w eid]
  (let [xy' (search-new-player-pos w eid)]
    (-> w
        (conj [eid :position xy'])
        (conj [eid :position-tts (inc (w eid :position-tts 0))])
        (conj [eid :velocity nil])
        (conj [eid :score (dec (w eid :score 0))]))))


(defn- kill-entity [w eid]
  (if (w eid :player)
    ;; FIXME: use multimethods or protocols here
    (kill-player w eid)
    (remove-entity w eid)))


(defn sys-kill-collided-entities [w]
  (reduce
   (fn [w' eid]
     (let [et (w eid :type)
           cw (w eid :collide-with)
           cwt (map #(w % :type) cw)]
       ;; TODO: move out, use multimethods/protocols?
       (cond
         ;; everything kills player except own bullets
         (and (= et :player)
              (some #(not= eid (:owner (w % :bullet))) cw))
         (let [all-bullets-owners (keep #(:owner (w % :bullet)) cw)
               bullets-owners (remove #(= eid %) all-bullets-owners)]
           (as-> w' $
             (reduce inc-player-score $ bullets-owners)
             (kill-player $ eid)))

         ;; everything kills bullet except other bullets & players
         (and (= et :bullet) (not-any? #{:bullet :player} cwt))
         (kill-entity w' eid)

         :else w')))
   w
   (entities w :collide-with)))


(defn- entity-out-of-gamefield? [w eid]
  (when-let [xy (w eid :position)]
    (let [[x y] ((juxt :x :y) xy)]
      (> (v2/mag xy) gm/world-radius))))


(defn sys-kill-entities-out-of-gamefield [w]
  (transduce
   (filter #(entity-out-of-gamefield? w %))
   (completing kill-entity)
   w
   (entities w :position)))


(defn sys-kill-outdated-entities [w now]
  (transduce
   (filter #(<= (w % :sched-kill-at) now))
   (completing kill-entity)
   w
   (entities w :sched-kill-at)))


(defn kill-entity-at [w eid at]
  (let [a (w eid :sched-kill-at)]
    (if (or (nil? a) (< at a))
      (conj w [eid :sched-kill-at at])
      w)))
