(ns pintoid.server.player
  (:use
   [pintoid.assets proto sprites sounds]
   [pintoid.ecs core system])
  (:require
   [pintoid.server.sound :as snd]
   [pintoid.server.vec2 :as v2]
   [taoensso.timbre :as timbre]))

(def engine-forward-force 0.08)
(def engine-reverse-force 0.02)
(def engine-rotate-speed 0.21)

(def camera-scale-factor 1)
(def camera-scale-vxy-pow -0.33)
(def camera-scale-vxy-add 3)
(def camera-scale-min 0.25)
(def camera-scale-max 1.25)
(def camera-scale-inertia 300)

(defn search-new-player-pos [w eid]
  (let [p (v2/vec2 (rand-int 2000) (rand-int 2000))]
    (timbre/debugf "Player %s pos %s" eid p)
    p))

(defn inc-player-score [w eid]
  (update-comp w eid :player update :score (fnil inc 0)))

(defn kill-player [w eid]
  (timbre/debugf "Kill & respawn player %s" eid)
  (let [xy' (search-new-player-pos w eid)]
    (-> w
        (put-comp eid :position xy')
        (put-comp eid :position-tts (inc (w eid :position-tts 0)))
        (put-comp eid :velocity nil)
        (update-comp eid :player update :score (fnil dec 0)))))

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
              angle' (+ angle (* rd engine-rotate-speed))
              ef (case ed -1 (- engine-reverse-force) 1 engine-forward-force 0)
              fxy (v2/from-polar ef angle')
              snd (if (= ed 0)
                    #(snd/stop-sound! % eid :engine)
                    #(snd/play-sound! % eid :engine engine-sound))]
          (timbre/tracef "Update player angle to %s, self-fxy %s" angle' fxy)
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
           (timbre/debugf "Spawn bullet for %s" eid)
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

(defn calc-camera-scale-factor [v]
  (-> v
      (+ camera-scale-vxy-add)
      (Math/pow camera-scale-vxy-pow)
      (* camera-scale-factor)
      (max camera-scale-min)
      (min camera-scale-max)))

(defn asys-udpate-cameras [w now]
  (run-timed-system
   w now
   (fn [dt]
     (combine-systems
      (each-entity w eid [_ :player
                          p :position
                          v :velocity
                          [_ _ s1] [:camera [0 0 1]]]
        (let [s2 (calc-camera-scale-factor (v2/mag v))
              a (min 1 (/ (double dt) camera-scale-inertia))
              b (- 1 a)
              s' (+ (* a s2) (* b s1))]
          (fn-> (put-comp eid :camera [(:x p) (:y p) s']))))))))

(defn remove-player [w eid]
  (remove-entity w eid))

(defn add-new-player [w eid {:keys [score nick] :or {score 0}}]
  (let [xy (search-new-player-pos w eid)]
    (add-entity w eid (player :position xy :nick nick :score score))))

(defn process-uinput [w eid uinput]
  (put-comp w eid ::user-input uinput))
