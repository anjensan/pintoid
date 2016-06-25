(ns pintoid.client.cs-comm
  (:use
   [pintoid.client.engine :only
    [update-world-snapshot!
     update-player-state!]]
   [pintoid.client.animation :only
    [defer-action!]]
   [pintoid.client.utils :only [panic! limit-str]])
  (:require
   [chord.client :refer [ws-ch]]
   [cljs.core.async :refer [<! >! close! timeout]])
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]]
   [pintoid.client.utils :refer [log]]))


(def client-server-time-diff 0)
(def client-server-ping 0)

(def user-input-update-delay 50)

;; client <> server websocket
(def server-ws-channel nil)
(def websocket-url (let [wl js/window.location] (str "ws://" (.-host wl) "/ws")))


(declare receive-server-messages)

(defn init-cs-communication []
  (go
    (log :info "init cs communication")
    (let [{:keys [ws-channel error]}
          (<! (ws-ch websocket-url {:format :json-kw}))]
      (if error
        (panic! error)
        (do
          (set! server-ws-channel ws-channel)
          (receive-server-messages ws-channel))))))


;; == Send to server

(defn send-message-to-server [msg]
  (when-let [c server-ws-channel]
    (log :debug "send to server:" (limit-str 120 msg))
    (go (>! c msg))))


(defn spawn-user-input-sender [make-user-input-snapshot-fn]
  (go-loop []
    (let [ui (make-user-input-snapshot-fn)]
      (send-message-to-server {:command :user-input :data ui}))
    (<! (timeout user-input-update-delay))
    (recur)))


;; == Receive from server

(defmulti handle-server-message (comp keyword :command))

(defn receive-server-messages [ws-chan]
  (log :info "receive messages from ws-socket")
  (go-loop []
    (when-let [{:keys [message error]} (<! ws-chan)]
      (if error
        (panic! error)
        (do
          (log :debug "server msg:" (limit-str 120 message))
          (handle-server-message message)
          (recur))))))


(defmethod handle-server-message :default [msg]
  (log :info "unknown server message" msg))



(defmethod handle-server-message :ping [m]
  ;; TODO: implement
  .)


(defmethod handle-server-message :snapshot [m]
  (let [{:keys [at game entts-json]} m]
    (set! client-server-time-diff
          (-
           (+ at (/ client-server-ping 2))
           (js/performance.now)))
    (defer-action! update-world-snapshot! at game (js/JSON.parse entts-json))))


(defmethod handle-server-message :init-player [m]
  (update-player-state! (:player m)))
