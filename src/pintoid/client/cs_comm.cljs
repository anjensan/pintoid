(ns pintoid.client.cs-comm
  (:use
   [pintoid.client.utils :only [panic! limit-str]])
  (:require
   [chord.client :refer [ws-ch]]
   [cljs.core.async :refer [<! >! close! timeout]])
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]]
   [pintoid.client.utils :refer [log-info log-debug]]))


;; client <> server websocket
(def server-ws-channel nil)
(def websocket-url
  (let [wl js/window.location]
    (str "ws://" (.-host wl) "/ws")))


(defmulti handle-server-message :cmd)


(defmethod handle-server-message :default [msg]
  (println "unknown server message" msg))


(defn receive-server-messages [ws-chan]
  (log-info "receive messages from ws-socket")
  (go-loop []
    (when-let [{:keys [message error]} (<! ws-chan)]
      (if error
        (panic! error)
        (do
          (log-debug "server msg:" (limit-str 120 message))
          (handle-server-message message)
          (recur))))))


(defn send-message-to-server [msg]
  (when-let [c server-ws-channel]
    (log-debug "send to server:" (limit-str 120 msg))
    (go (>! c msg))))


(defn init-cs-communication []
  (go
    (log-info "init cs communication")
    (let [{:keys [ws-channel error]}
          (<! (ws-ch websocket-url {:format :json}))]
      (if error
        (panic! error)
        (do
          (set! server-ws-channel ws-channel)
          (receive-server-messages ws-channel))))))

;; ---

(defmethod handle-server-message :ping [m]
  :...)
