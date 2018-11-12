(ns pintoid.server.game.core
  (:use
   [pintoid.server.data core consts]
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

(defstate last-stable-world
  :start (atom nil))

(defn world-error-handler [world e]
  (clojure.stacktrace/print-stack-trace e))

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
  (add-entity w time-eid {::time now}))

(defn- sys-fixate-world-state [w]
  (reset! last-stable-world w))

(defn get-world []
  (let [w (or @last-stable-world @world)]
    [(world-time w) w]))

(defn game-remove-player [eid]
  (send world remove-player eid))

(defn game-add-new-player [eid]
  (send world add-new-player eid))

(defn game-process-user-input [eid user-input]
  (send world process-uinput eid user-input))

(defn- current-time [w]
  (System/currentTimeMillis))

(defn profile-asys
  ([as-var]
   (profile-asys (-> as-var meta :name) as-var))
  ([n as]
   (fn [w & rs]
     (let [d (promise)
           f (tufte/p [:run n]
              (let [z (apply as w rs)]
                (deliver d :done)
                z))]
       (fn [w']
         (tufte/p [:wait n] @d)
         (tufte/p [:fixup n] (f w'))
         )))))

(defn asys-fork-join []
  (let [[fork' join] (ecss/asys-fork-join)
        fork (fn [w id as & rs] (apply fork' w id (profile-asys as) rs))]
    [fork join]))

(defn- sys-world-tick [w]
  (tufte/profile
   {:dynamic? true}
   (tufte/p
    {:id :sys-world-tick}
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

(defn game-world-tick []
  (send-off world sys-world-tick))

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
  (tufte/profile
   {:dynamic? true}
   (tufte/p
    :dump-the-world
    (let [vf (comp (memoize (visible-by-player? w pid max-user-view-distance)) key)]
      (ecsd/dumps-map
       :self-player  (dump-self-player pid)
       :asset        (dumpc w :asset)
       :score        (dumpc w :score)
       :position     (dumpc w :position, :filter vf, :map v2/to-vec)
       :position-tts (dumpc w :position-tts, :filter vf)
       :sprite       (dumpc w :sprite, :filter vf)
       :layer        (dumpc w :layer, :filter vf)
       :angle        (dumpc w :angle, :filter vf)
       :type         (dumpc w :type, :filter vf)
       :sound        (dumpc w :sound, :filter vf)
       )))))
