(ns pintoid.server.game.core
  (:use
   [pintoid.server.ecs core data dump]
   [pintoid.server.game physics collide kill player]
   [pintoid.server vec2 game-maps])
  (:require
   clojure.stacktrace
   [mount.core :refer [defstate]]
   [taoensso.timbre :as timbre]
   [pintoid.server.game-maps :as gm]
   ))

(defstate last-stable-world
  :start (atom nil))

(defn world-error-handler [world e]
  (clojure.stacktrace/print-stack-trace e))

(defstate world
  :start (do
           (timbre/debug "Init world agent")
           (send (agent (create-ecs) :error-handler world-error-handler) add-game-entities))
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
      (sys-spawn-bullets now)
      (sys-change-engine-based-on-ui now)
      (sys-kill-outdated-entities now)
      (sys-simulate-physics now)
      (sys-collide-entities)
      (sys-kill-collided-entities)
      (sys-kill-entities-out-of-gamefield)
      (sys-attach-world-time now)
      (sys-fixate-world-state)
      )))

(defn game-world-tick []
  (send-off world sys-world-tick))


(defn- dump-self-player [pid]
  (fn [s] [(when-not s {pid true}) true]))

(defn dump-the-world [w pid]
  (let [v? #(when-let [f (w % :visible?)] (f w pid %))
        eids (into #{} (filter v?) (entities w :position))
        vf (fn [[eid _]] (contains? eids eid))]
    (dumps-map
     :self-player  (dump-self-player pid)
     :assets       (dump w :assets)
     :score        (dump w :score)
     :position     (dump w :position, :filter vf, :map to-vec)
     :position-tts (dump w :position-tts, :filter vf)
     :sprite       (dump w :sprite, :filter vf)
     :layer        (dump w :layer, :filter vf)
     :angle        (dump w :angle, :filter vf)
     :type         (dump w :type, :filter vf)
     )))
