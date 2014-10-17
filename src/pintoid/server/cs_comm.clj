(ns pintoid.server.cs-comm
  (:use [pintoid.server.game])
  (:require
   [clojure.core.async :refer
    [<! >! put! close! go-loop go chan timeout]]
   [cheshire.core :as json]
   ))


(def client-chans (atom {}))
(def clients-counter (atom 0))


(defn send-message-to-clients [pids m]
  (let [cc @client-chans
        pl (or pids (keys cc))]
    (doseq [ch (map cc pl)]
      (go (<! ch m)))))


(defmulti handle-client-message
  (fn [pid m] (keyword (:cmd m))))


(defmethod handle-client-message :default [pid m]
  (println "unknown message" m "from" pid))


(defn drop-client-connection [pid]
  (when-let [ch (@client-chans pid)]
    (close! ch))
  (swap! client-chans dissoc pid))


(defn handle-client-error [pid cmd error]
  (handle-client-message pid {:cmd cmd :error error}))


(defn spawn-wschan-reading-loop [pid ws-channel]
  (go-loop []
    (if-let [{:keys [message error] :as msg} (<! ws-channel)]
      (do
        (if message
          (do
            (handle-client-message pid message)
            (recur))
          (handle-client-error pid :failure error)))
      (handle-client-error pid :disconnect nil))))


(defn add-new-client-connection [req]
  (let [pid (swap! clients-counter inc)
        ws-channel (:ws-channel req)]
    (swap! client-chans assoc pid ws-channel)
    (spawn-wschan-reading-loop pid ws-channel)
    (handle-client-message pid {:cmd :connected :req req})
    pid))


;; ---

(defn spawn-player-notifier-loop [pid]
  (go-loop []
    ;; 20x per second
    (<! (timeout 50))
    (let [ss (create-game-snapshot pid)]
      (send-message-to-clients
       [pid]
       {:cmd :snapshot
        :data (json/encode ss)}))
    (when (@client-chans pid)
      (recur))))


;; --- handlers here!

(defmethod handle-client-message :failure [pid params]
  (println "client failure" (:error params)))


(defmethod handle-client-message :connected [pid m]
  (println "new player" pid)
  ;; TODO: add player to game
  )


(defmethod handle-client-message :disconnect [pid _]
  (println "player" pid "disconnected")
  ;; TODO: remove player from game
  (drop-client-connection pid))


(defmethod handle-client-message :move-player [pid m]
  (println "move player" pid "by", (:dx m 0) (:dy m 0))
  ;; TODO: move player
  )
