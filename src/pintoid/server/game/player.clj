(ns pintoid.server.game.player
  (:use
   [pintoid.server utils ecs])
  (:require
   [pintoid.server.vec2 :as v2]
   [taoensso.timbre :as timbre]
   [pintoid.server.game-maps :as gm]))


(defn search-new-player-pos [w eid]
  (v2/vec2 (rand-int 2000) (rand-int 2000)))


(defn inc-player-score [w eid]
  (conj w [eid :score (inc (w eid :score 0))]))


(def sys-change-engine-based-on-ui
  (make-timed-system
   (fn [w dt]
     (into
      w
      (mapcat
       (fn [eid]
         (let [ui (w eid :user-input)
               rd (:rotate-dir ui 0)
               angle (w eid :angle 0)
               angle' (+ angle (* rd gm/rotate-speed))
               ed (:engine-dir ui)
               ef (case ed -1 (- gm/engine-reverse-force) 1 gm/engine-forward-force 0)
               fxy (v2/from-polar angle' ef)]
           [[eid :self-fxy fxy]
            [eid :angle angle']])))
      (eids$ w [:* :player :user-input])))))


(defn sys-spawn-bullets [w now]
  (reduce
   (fn [w' eid]
     (or
      (let [ui (w eid :user-input)]
        (when (or (:fire? ui) (:alt-fire? ui))
          (let [b-proto (if (:fire? ui) gm/bullet gm/bullet-alt)
                b (:bullet b-proto)
                b-cooldown (:cooldown b)
                last-fire-at (w eid :last-fire-at)]
            (when (or (nil? last-fire-at) (< (+ last-fire-at b-cooldown) now))
              (let [xy (w eid :position)
                    vxy (w eid :velocity v2/zero)
                    angle (w eid :angle)
                    b-vxy (v2/v+ vxy (v2/from-polar angle (:velocity b)))
                    b-xy xy
                    b-lifetime (:lifetime b)]
                (-> w'
                    (put-component eid :last-fire-at now)
                    (add-new-entity
                      (assoc b-proto
                        :position b-xy
                        :velocity b-vxy
                        :sched-kill-at (+ now b-lifetime)
                        :bullet (assoc gm/bullet :owner eid)
                        :angle angle))))))))
      w'))
   w
   (eids$ w [:* :player :user-input])))


(defn remove-player [w eid]
  (drop-entity w eid))


(defn add-new-player [w eid]
  (let [xy (search-new-player-pos w eid)]
    (add-entity w eid (assoc gm/player :player true, :position xy))))


(defn process-uinput [w eid uinput]
  (conj w [eid :user-input uinput]))
