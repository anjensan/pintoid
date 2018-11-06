(ns pintoid.server.game.kill
  (:use
   [pintoid.server.ecs core]
   [pintoid.server.data consts]
   [pintoid.server.game player])
  (:require
   [pintoid.server.vec2 :as v2]
   [taoensso.timbre :as timbre]))

(defn- kill-player [w eid]
  (let [xy' (search-new-player-pos w eid)]
    (-> w
        (put-comp eid :position xy')
        (put-comp eid :position-tts (inc (w eid :position-tts 0)))
        (put-comp eid :velocity nil)
        (update-comp eid :score (fnil dec 0)))))

(defn- kill-entity [w eid]
  (if (w eid :player)
    ;; FIXME: use multimethods or protocols here
    (kill-player w eid)
    (remove-entity w eid)))

(defn sys-kill-collided-entities [w]
  (entities-reduce w :collide-with
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
         (kill-entity w' eid))))))

(defn- entity-out-of-gamefield? [w eid]
  (when-let [xy (w eid :position)]
    (let [[x y] ((juxt :x :y) xy)]
      (> (v2/mag xy) world-kill-radius))))

(defn sys-kill-entities-out-of-gamefield [w]
  (entities-reduce w :position
                    (filter #(entity-out-of-gamefield? w %))
                    kill-entity))

(defn sys-kill-outdated-entities [w now]
  (entities-reduce w :sched-kill-at
    (filter #(<= (w % :sched-kill-at) now))
    kill-entity))

(defn kill-entity-at [w eid at]
  (let [a (w eid :sched-kill-at)]
    (if (or (nil? a) (< at a))
      (put-comp w eid :sched-kill-at at)
      w)))
