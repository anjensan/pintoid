(ns pintoid.server.cs-comm
  (:use [pintoid.server game utils])
  (:require
   [clojure.core.async :refer
    [<! >! <!! >!! put! close! thread go chan go-loop]]
   [cheshire.core :as json]
   [clojure.set :refer [union]]
   ))

(def clients-notify-delay 500)           ; 2x per second

(def client-chans (atom {}))
(def clients-counter (atom 0))
(def client-notifier-agents (atom {}))


(defn send-message-to-clients [pids message]
  (let [cc @client-chans
        pl (or pids (keys cc))]
    (doseq [pid (or pids (keys cc))]
      (when-let [ch (cc pid)]
        (log-debug "pid" pid ">>" message)
        (go (>! ch message))))))


(defn send-command-to-clients [pids command message]
  (send-message-to-clients pids (assoc message :cmd command)))


(defmulti handle-client-message
  (fn [pid m] (keyword (:cmd m))))


(defmethod handle-client-message :default [pid m]
  (println "unknown message" m "from" pid))


(defn drop-client-connection [pid]
  (when-let [ch (@client-chans pid)]
    (close! ch))
  (swap! client-notifier-agents dissoc pid)
  (swap! client-chans dissoc pid))


(defn handle-client-error [pid cmd error]
  (handle-client-message pid {:cmd cmd :error error}))


(defn spawn-wschan-reading-loop [pid ws-channel]
  (go-loop []
    (if-let [{:keys [message error] :as msg} (<! ws-channel)]
      (do
        (if message
          (do
            (log-debug "pid" pid "<<" message)
            (handle-client-message pid message)
            (recur))
          (handle-client-error pid :failure error)))
      (handle-client-error pid :disconnect nil))))


(defn add-new-client-connection [req]
  (let [pid (swap! clients-counter inc)
        ws-channel (:ws-channel req)]
    (swap! client-notifier-agents assoc pid (agent {:pid pid}))
    (swap! client-chans assoc pid ws-channel)
    (spawn-wschan-reading-loop pid ws-channel)
    (handle-client-message pid {:cmd :connected :req req})
    pid))


;; ---

(defn create-and-send-world-snapshot-agent-upd [pss w pid]
  (let [at (:at w)
        gs (take-game-snapshot w pid)
        client-eids (:client-eids pss #{})
        snapshot (take-entities-snapshot w pid client-eids)
        rem-eids (map :eid (:rem snapshot))
        add-eids (map :eid (:add snapshot))
        new-c-eids (->>
                    client-eids
                    (remove (set rem-eids))
                    (union (set add-eids))
                    (set))
        ]
    (send-message-to-clients
     [pid]
     {:cmd :snapshot
      :at at
      :game gs
      :entts-json (json/encode snapshot)
      })
    (assoc pss :client-eids new-c-eids)))

    
(defn send-world-snapshot-to-client [w pid]
  (when-let [a (@client-notifier-agents pid)]
    (send-off a create-and-send-world-snapshot-agent-upd w pid)))


(defn send-snapshots-to-all-clients []
  (let [w (fix-world-state)]
    (log-debug "send snapshot, time" (:at w))
    (doseq [pid (keys @client-chans)]
      (send-world-snapshot-to-client w pid))))


;; --- handlers here!

(defmethod handle-client-message :failure [pid params]
  (println "client failure" (:error params)))


(defmethod handle-client-message :connected [pid m]
  (log-info "new player" pid)
  ;; TODO: add player to game
  )


(defmethod handle-client-message :disconnect [pid _]
  (log-info "player" pid "disconnected")
  ;; TODO: remove player from game
  (drop-client-connection pid))


(defmethod handle-client-message :move-player [pid m]
  (log-debug "move player" pid "by", (:dx m 0) (:dy m 0))
  ;; TODO: move player
  )
