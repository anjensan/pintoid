(ns pintoid.server.game.player
  (:use
   [pintoid.server.data consts proto assets sounds]
   [pintoid.server.ecs core system entity])
  (:require
   [pintoid.server.game.sound :as snd]
   [pintoid.server.vec2 :as v2]
   [taoensso.timbre :as timbre]))


(defn search-new-player-pos [w eid]
  (v2/vec2 (rand-int 2000) (rand-int 2000)))

(defn inc-player-score [w eid]
  (update-comp w eid :score (fnil inc 0)))

(defn asys-change-engine-based-on-ui [w now]
  (run-timed-system
   w now
   (fn [dt]
     (combine-systems!
      (each-entity w eid [_ :player
                          ui ::user-input
                          angle [:angle 0]
                          ]
        (let [rd (:rotate-dir ui 0)
              ed (:engine-dir ui)
              angle' (+ angle (* rd rotate-speed))
              ef (case ed -1 (- engine-reverse-force) 1 engine-forward-force 0)
              fxy (v2/from-polar ef angle')
              snd (if (= ed 0)
                    #(snd/stop-sound! % eid :engine)
                    #(snd/play-sound! % eid :engine engine-sound))]
          (fn->
           (snd)
           (put-comp! eid :self-fxy fxy)
           (put-comp! eid :angle angle'))))))))

(defn asys-spawn-bullets [w now]
  (combine-systems
   (each-entity w eid [_ :player
                       ui ::user-input
                       fc [:fire-cooldown 0]]
     (when (and (or (nil? fc) (< fc now))
                (or (:fire? ui) (:alt-fire? ui)))
       (let-entity w eid [xy :position
                          vxy [:velocity v2/zero]
                          angle :angle]
         (let [b-proto (if (:fire? ui) (bullet) (bullet-alt))
               b (:bullet b-proto)
               b-vxy (v2/v+ vxy (v2/from-polar (:velocity b) angle))
               b-xy xy
               bid (next-entity)]
           (fn->
            (put-comp eid :fire-cooldown (+ now (:cooldown b)))
            (add-entity
             bid
             (assoc b-proto
                    :sched-kill-at (+ now (:lifetime b))
                    :position b-xy
                    :velocity b-vxy
                    :bullet (assoc b :owner eid)
                    :angle angle))
            (snd/play-sound bid (rand-nth bullet-bang-sounds))
            )))))))

(defn remove-player [w eid]
  (remove-entity w eid))

(defn add-new-player [w eid]
  (let [xy (search-new-player-pos w eid)]
    (add-entity w eid (player :position xy))))

(defn process-uinput [w eid uinput]
  (put-comp w eid ::user-input uinput))
