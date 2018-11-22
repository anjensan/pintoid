(ns pintoid.server.game.core
  (:use
   [pintoid.assets core]
   [pintoid.server.game physics collide kill player sound]
   [pintoid.server vec2])
  (:require
   clojure.stacktrace
   [clojure.algo.monads :as m]
   [pintoid.server.entity :refer :all]
   [pintoid.server.ecs
    [core :refer :all]
    [system :as ecss]
    [dump :as ecsd]
    [data :refer [create-ecs]]]
   [taoensso.tufte :as tufte]
   [pintoid.server.vec2 :as v2]
   [mount.core :refer [defstate]]
   [taoensso.timbre :as timbre]
   ))

(def max-user-view-distance 2500)

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

(defn asys-fork-join []
  (let [[fork' join] (ecss/asys-fork-join)
        fork (fn [w id as & rs] (apply fork' w id (profile-asys as) rs))]
    [fork join]))

(defn- sys-world-tick [w]
  (timbre/tracef "== Next world tick... ==")
  (tufte/profile {:dynamic? true}
   (tufte/p :sys-world-tick
    (let [now (current-time w)
          [fork join] (asys-fork-join)]
      (-> w
          (sys-attach-world-time now)
          (fork :actualize-protos #'asys-actualize-entity-protos)

          (fork :sounds #'asys-garbage-sounds now)
          (fork :kill-outdated #'asys-kill-outdated-entities now)
          (fork :kill-out-of-gamefield #'asys-kill-entities-out-of-gamefield)
          (fork :engine #'asys-change-engine-based-on-ui now)

          (join :actualize-protos)

          (fork :physics-vxy #'asys-physics-update-vxy now)
          (fork :physics-move #'asys-physics-move now)

          (fork :physics-bound #'asys-physics-bound-circle now)

          (fork :collide #'asys-collide-entities)
          (join :collide)

          (join :kill-outdated)
          (join :sounds)
          (join :engine)
          (join :kill-out-of-gamefield)

          (join :physics-bound)
          (join :physics-move)

          (fork :kill-collided #'asys-kill-collided-entities)

          (join :physics-vxy)

          (fork :bullets #'asys-spawn-bullets now)
          (join :bullets)
          (join :kill-collided)

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

(defn dump-the-world [w pid]
  (timbre/tracef "Dump world for player %s" pid)
  (tufte/profile
   {:dynamic? true}
   (tufte/p
    :dump-the-world
    (let [vf (comp (memoize (visible-by-player? w pid max-user-view-distance)) key)]
      (ecsd/dumps-map
       :self-player  (dump-self-player pid)
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
