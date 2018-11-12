(ns pintoid.server.game.kill
  (:use
   [pintoid.server.ecs core system]
   [pintoid.server.data consts]
   [pintoid.server.game player])
  (:require
   [pintoid.server.vec2 :as v2]
   [taoensso.timbre :as timbre]))

(defn- kill-entity [w eid]
  (timbre/debugf "Kill collided entity %s, type %s" eid (get-comp w eid :type))
  (if (w eid :player)
    ;; FIXME: use multimethods or protocols here
    (kill-player w eid)
    (remove-entity w eid)))

(defn- inc-players-score [w ps]
  (reduce inc-player-score w ps))

(defn asys-kill-collided-entities [w]
  (combine-systems
   (each-entity w eid [cw :collide-with
                       et :type]
     (when-let [cwt (seq (map #(w % :type) cw))]

       ;; TODO: move out, use multimethods/protocols?
       (cond
         ;; everything kills player except own bullets
         (and (= et :player) (some #(not= eid (:owner (w % :bullet))) cw))
         (let [all-bullets-owners (keep #(:owner (w % :bullet)) cw)
               bullets-owners (remove #(= eid %) all-bullets-owners)]
           (fn->
            (inc-players-score bullets-owners)
            (kill-player eid)))

         ;; everything kills bullet except other bullets & players
         (and (= et :bullet) (not-any? #{:bullet :player} cwt))
         (fn-> (kill-entity eid))

         )))))

(defn- entity-out-of-gamefield? [w eid]
  (when-let [xy (w eid :position)]
    (let [[x y] ((juxt :x :y) xy)]
      (> (v2/mag xy) world-kill-radius))))

(defn asys-kill-entities-out-of-gamefield [w]
  (combine-systems
   (each-entity w eid [p :position]
     (when (entity-out-of-gamefield? w p)
       (timbre/tracef "Kill out-of-field entity %s" eid)
       (fn-> (kill-entity eid))))))

(defn asys-kill-outdated-entities [w now]
  (combine-systems
   (each-entity w eid [t :sched-kill-at]
     (when (<= t now)
       (timbre/tracef "Kill outdated entity %s" eid)
       (fn-> (kill-entity eid))))))

(defn kill-entity-at [w eid at]
  (let [a (w eid :sched-kill-at)]
    (if (or (nil? a) (< at a))
      (put-comp w eid :sched-kill-at at)
      w)))
