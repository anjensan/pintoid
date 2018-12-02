(ns pintoid.server.game
  (:use
   [pintoid.assets core]
   [pintoid.server physics collide kill player sound vec2])
  (:require
   clojure.stacktrace
   [clojure.algo.monads :as m]
   [pintoid.ecs.entity :refer :all]
   [pintoid.ecs
    [core :refer :all]
    [system :as ecss]
    [dump :as ecsd]
    [data :refer [create-ecs]]]
   [taoensso.tufte :as tufte]
   [pintoid.server.vec2 :as v2]
   [mount.core :refer [defstate]]
   [taoensso.timbre :as timbre]
   ))

(def dev-asystems (atom {}))

(def max-user-view-distance 3000)

(defstate last-stable-world
  :start (atom nil))

(defn world-error-handler [world e]
  (timbre/error e "world error"))

(defstate world
  :start (do
           (timbre/debug "Init world agent")
           (send (agent (create-ecs) :error-handler world-error-handler)
                 load-game-entities))
  :stop (do
          (timbre/debug "Stop world agent")
          (send world (constantly ::destroyed))))

(def ^:private time-eid (next-entity))

(defn- world-time [w]
  (w time-eid ::time))

(defn- sys-attach-world-time [w now]
  (timbre/tracef "Atatch time %s to world" now)
  (add-entity w time-eid {::time now}))

(defn- sys-fixate-world-state [w]
  (timbre/tracef "Fixate world state")
  (reset! last-stable-world w))

(defn get-world []
  (let [w (or @last-stable-world @world)]
    [(world-time w) w]))

(defn game-remove-player [eid]
  (timbre/debugf "Remove player %s" eid)
  (send world remove-player eid))

(defn game-add-new-player [eid {:keys [nick]}]
  (timbre/debugf "Add new player %s: %s" eid nick)
  (send world add-new-player eid {:nick nick}))

(defn game-process-user-input [eid user-input]
  (timbre/tracef "User input from %s: %s" eid user-input)
  (send world process-uinput eid user-input))

(defn- current-time [w]
  (System/currentTimeMillis))

(defn profile-asys
  ([as-var]
   (profile-asys (-> as-var meta :name) as-var))
  ([n as]
   (fn [w & rs]
     (timbre/tracef "Spawn system %s" n)
     (let [f (tufte/p [:run n] (apply as w rs))]
       (fn [w']
         (timbre/tracef "Fixup system %s" n)
         (tufte/p [:fixup n] (f w'))
         )))))

(defn sys-developer-tools [w]
  (let [ds @dev-asystems
        [fork join] (ecss/asys-fork-join)]
    (as-> w w
      (reduce #(fork %1 (key %2) (val %2)) w ds)
      (reduce #(join %1 (key %2)) w ds))))

(defn- sys-world-tick [w]
  (timbre/tracef "== Next world tick... ==")
  (tufte/profile {:dynamic? true}
   (tufte/p :sys-world-tick
    (let [now (current-time w)
          [fork' join] (ecss/asys-fork-join)
          fork (fn [w id as & rs] (apply fork' w id (profile-asys id as) rs))]
      (-> w
          (sys-attach-world-time now)

          (fork :collide #'asys-collide-entities)
          (fork :physics-vxy #'asys-physics-update-vxy now)
          (fork :physics-move #'asys-physics-move now)
          (fork :physics-bound #'asys-physics-bound-circle now)

          (fork :handle-ui #'asys-change-engine-based-on-ui now)
          (fork :sounds #'asys-garbage-sounds now)
          (fork :kill-outdated #'asys-kill-outdated-entities now)
          (fork :kill-out-of-gamefield #'asys-kill-entities-out-of-gamefield)

          (join :kill-outdated)
          (join :kill-out-of-gamefield)
          (join :physics-move)
          (join :sounds)
          (join :handle-ui)
          (join :physics-bound)
          (join :physics-vxy)
          (join :collide)

          (fork :kill-collided #'asys-kill-collided-entities)
          (fork :bullets #'asys-spawn-bullets now)
          (fork :camera #'asys-udpate-cameras now)

          (join :kill-collided)
          (join :bullets)
          (join :camera)

          (sys-developer-tools)
          (sys-fixate-world-state)
          )))))

(defn game-world-tick [done]
  (send-off world sys-world-tick)
  (send world (fn [w] (done) w)))

(defn- dump-self-player [pid]
  (fn [s] [(when-not s {pid true}) true]))

(defn- visible-by-player? [w pid max-dist]
  (let-entity w pid [pp :position]
    (fn [eid]
      (let-entity w eid [fow [:fog-of-war false]
                         pos :position]
        (or
         (not fow)
         (< (v2/dist pos pp) max-dist))))))

(defn dumpc [w c & rs]
  (tufte/p [:dump c]
   (m/domonad m/state-m [d (apply ecsd/dumpc w c rs)] (vec d))))

(defn dumpp [w p c]
  (fn [s]
    (let [v (get-comp w p c)]
      (if (= s v)
        [nil v]
        [{p v} v]))))

(defn dump-the-world [w pid]
  (timbre/tracef "Dump world for player %s" pid)
  (tufte/profile
   {:dynamic? true}
   (tufte/p
    :dump-the-world
    (let [vf (comp (memoize (visible-by-player? w pid max-user-view-distance)) key)]
      (ecsd/dumps-map
       :self-player  (dump-self-player pid)
       :camera       (dumpp w pid :camera)
       :player       (dumpc w :player)
       :asset        (dumpc w :asset)
       :position     (dumpc w :position, :filter vf, :map v2/to-vec)
       :position-tts (dumpc w :position-tts, :filter vf)
       :sprite       (dumpc w :sprite, :filter vf)
       :layer        (dumpc w :layer, :filter vf)
       :angle        (dumpc w :angle, :filter vf)
       :type         (dumpc w :type, :filter vf)
       :sound        (dumpc w :sound, :filter vf)
       )))))
