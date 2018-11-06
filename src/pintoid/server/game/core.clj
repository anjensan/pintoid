(ns pintoid.server.game.core
  (:use
   [pintoid.server.ecs core data dump entity system]
   [pintoid.server.data core consts]
   [pintoid.server.game physics collide kill player sound]
   [pintoid.server vec2])
  (:require
   clojure.stacktrace
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

(defn- sys-world-tick [w]
  (let [now (current-time w)]
    (-> w
      ((asys->sys asys-actualize-entity-protos))
      (sys-spawn-bullets now)
      ((asys->sys asys-change-engine-based-on-ui) now)
      (sys-kill-outdated-entities now)
      ((asys->sys asys-simulate-physics) now)
      ((asys->sys asys-physics-bound-circle) now)
      (sys-collide-entities)
      (sys-kill-collided-entities)
      (sys-kill-entities-out-of-gamefield)
      (sys-attach-world-time now)
      ((asys->sys asys-garbage-sounds) now)
      (sys-fixate-world-state)
      )))

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

(defn dump-the-world [w pid]
  (let [vf (comp (memoize (visible-by-player? w pid max-user-view-distance)) key)]
    (dumps-map
     :self-player  (dump-self-player pid)
     :asset        (dump w :asset)
     :score        (dump w :score)
     :position     (dump w :position, :filter vf, :map v2/to-vec)
     :position-tts (dump w :position-tts, :filter vf)
     :sprite       (dump w :sprite, :filter vf)
     :layer        (dump w :layer, :filter vf)
     :angle        (dump w :angle, :filter vf)
     :type         (dump w :type, :filter vf)
     :sound        (dump w :sound, :filter vf)
     )))
