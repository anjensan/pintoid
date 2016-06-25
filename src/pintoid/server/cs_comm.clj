(ns pintoid.server.cs-comm
  (:use [pintoid.server game utils ecs])
  (:require
   [clojure.core.async :refer
    [<! >! <!! >!! put! close! thread go chan go-loop]]
   [cheshire.core :as json]
   [clojure.tools.logging :as log]
   [clojure.set :refer [union]]))

(def client-chans (atom {}))

;; TODO: Replace with 'client-avatar-agents'
(def client-notifier-agents (atom {}))


(defn send-message-to-clients [pids message]
  (let [cc @client-chans
        pl (or pids (keys cc))]
    (doseq [eid (or pids (keys cc))]
      (when-let [ch (cc eid)]
        (log/trace "eid" eid ">>" message)
        (go (>! ch message))))))


(defn send-command-to-clients [pids command message]
  (send-message-to-clients pids (assoc message :command command)))


(defmulti handle-client-message #(-> %2 :command keyword))


(defmethod handle-client-message :default [eid m]
  (log/warn "unknown message" m "from" eid))


(defn drop-client-connection [eid]
  (when-let [ch (@client-chans eid)]
    (close! ch))
  (swap! client-notifier-agents dissoc eid)
  (swap! client-chans dissoc eid))


(defn handle-client-error [eid command error]
  (handle-client-message eid {:command command :error error}))


(defn spawn-wschan-reading-loop [eid ws-channel]
  (go-loop []
    (if-let [{:keys [message error] :as msg} (<! ws-channel)]
      (do
        (if message
          (do
            (log/trace "eid" eid "<<" message)
            (handle-client-message eid message)
            (recur))
          (handle-client-error eid :failure error)))
      (handle-client-error eid :disconnect nil))))


(defn add-new-client-connection [req]
  (let [eid (next-entity-id)
        ws-channel (:ws-channel req)]
    (swap! client-notifier-agents assoc eid (agent {:pid eid}))
    (swap! client-chans assoc eid ws-channel)
    (spawn-wschan-reading-loop eid ws-channel)
    (handle-client-message eid {:command :connected :req req})
    eid))


;; ---

(defn create-and-send-world-snapshot-agent-upd [pss w eid]
  (let [at (get-world-time w)
        gs (take-game-snapshot w eid)
        client-eids (:client-eids pss #{})
        snapshot (take-entities-snapshot w eid client-eids)
        rem-eids (:rem snapshot)
        add-eids (map :eid (:add snapshot))
        new-c-eids (->>
                    client-eids
                    (remove (set rem-eids))
                    (union (set add-eids))
                    (set))
        ]
    (send-message-to-clients
     [eid]
     {:command :snapshot
      :at at
      :game gs
      ;; :entts-json (json/encode snapshot)
      :entts snapshot
      })
    (assoc pss :client-eids new-c-eids)))


(defn send-world-snapshot-to-client [w eid]
  (when-let [a (@client-notifier-agents eid)]
    (send-off a create-and-send-world-snapshot-agent-upd w eid)))


(defn send-snapshots-to-all-clients []
  (let [w (fix-world-state)]
    (doseq [eid (keys @client-chans)]
      (send-world-snapshot-to-client w eid))))


;; --- handlers here!

(defmethod handle-client-message :failure [eid params]
  (log/warn "client failure" (:error params)))


(defmethod handle-client-message :connected [eid m]
  (log/info "new player" eid)
  (let [ps (game-add-new-player eid)]
    ;; XXX
    (send-message-to-clients [eid] {:command :init-player :player (player-init-obj eid ps)})))


(defmethod handle-client-message :disconnect [eid _]
  (log/info "player" eid "disconnected")
  (drop-client-connection eid)
  (game-remove-player eid))


(defmethod handle-client-message :user-input [eid m]
  (log/trace "user input player" m)
  (game-process-user-input eid (:data m)))
