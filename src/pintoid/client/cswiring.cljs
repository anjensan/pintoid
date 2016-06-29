(ns pintoid.client.cswiring
  (:use
   [pintoid.client.engine :only [update-world-snapshot!]]
   [pintoid.client.graphics.animloop :only [defer-action!]]
   [pintoid.client.utils :only [panic! limit-str]])
  (:require
   [chord.client :refer [ws-ch]]
   [cljs.core.async :refer [<! >! close! timeout]])
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]]
   [pintoid.client.macros :refer [log]]))


(def client-server-time-diff 0)
(def client-server-ping 0)

(def user-input-update-delay 50)

;; client <> server websocket
(def server-ws-channel nil)
(def websocket-url (let [wl js/window.location] (str "ws://" (.-host wl) "/ws")))


(declare receive-server-messages)
(declare on-channel-connected)

(defn init-cs-communication []
  (go
    (log :info "init cs communication")
    (let [{:keys [ws-channel error]}
          (<! (ws-ch websocket-url {:format :transit-json}))]
      (if error
        (panic! error)
        (do
          (set! server-ws-channel ws-channel)
          (receive-server-messages ws-channel)
          (on-channel-connected))))))


;; == Send to server

(defn send-message-to-server [msg]
  (when-let [c server-ws-channel]
    (log :debug "send to server:" (limit-str 120 msg))
    (go (>! c msg))))


(declare receive-message)

(defn receive-server-messages [ws-chan]
  (log :info "receive messages from ws-socket")
  (go-loop []
    (when-let [{:keys [message error]} (<! ws-chan)]
      (if error
        (panic! error)
        (do
          (log :debug "server msg:" (limit-str 120 message))
          (receive-message message)
          (recur))))))


(defn spawn-user-input-sender [make-user-input-snapshot-fn]
  (go-loop []
    (let [ui (make-user-input-snapshot-fn)]
      (send-message-to-server {:command :user-input :data ui}))
    (<! (timeout user-input-update-delay))
    (recur)))


;; == Receive from server

(defmulti receive-message (comp keyword :command))


(defn on-channel-connected []
  (send-message-to-server {:command :join-game}))


(defmethod receive-message :default [msg]
  (log :info "unknown server message" msg))


(defmethod receive-message :wpatch [m]
  (set! client-server-time-diff
        (- (+ (:time m) (/ client-server-ping 2))
           (js/performance.now)))
  (defer-action! update-world-snapshot! m))


