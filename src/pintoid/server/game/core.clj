(ns pintoid.server.game.core
  (:use
   [pintoid.server.game physics collide kill player]
   [pintoid.server utils math ecs game-maps])
  (:require
   [mount.core :refer [defstate]]
   [taoensso.timbre :as timbre]
   [pintoid.server.ecs :as ecs]
   [pintoid.server.game-maps :as gm]
   ))


(defstate last-stable-world
  :start (atom nil))

(defstate world
  :start (do
           (timbre/debug "Init world agent")
           (send
            (agent (create-ecs))
            #(reduce add-new-entity % (gm/game))))
  :stop (do
          (timbre/debug "Stop world agent")
          (send world (constantly ::destroyed))))


(def ^:private time-eid (next-entity-id))

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


