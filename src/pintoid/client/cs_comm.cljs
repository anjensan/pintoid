(ns pintoid.client.cs-comm
  (:use
   [pintoid.client.engine :only [update-world-snapshot!]]
   [pintoid.client.utils :only [panic! limit-str]])
  (:require
   [chord.client :refer [ws-ch]]
   [cljs.core.async :refer [<! >! close! timeout]])
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop]]
   [pintoid.client.utils :refer [log log-info log-debug]]))


;; client <> server websocket
(def server-ws-channel nil)
(def websocket-url
  (let [wl js/window.location]
    (str "ws://" (.-host wl) "/ws")))


(defmulti handle-server-message
  (fn [m] (keyword (:cmd m))))


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
          (<! (ws-ch websocket-url {:format :json-kw}))]
      (if error
        (panic! error)
        (do
          (set! server-ws-channel ws-channel)
          (receive-server-messages ws-channel))))))

;; ---

(defmethod handle-server-message :ping [m]
  :...)

(defmethod handle-server-message :snapshot [m]
  (let [{:keys [at game entts-json]} m
        entts (js/JSON.parse entts-json)]
    (log :trace ">>>>>" at game entts)
    (update-world-snapshot! at game entts)))
