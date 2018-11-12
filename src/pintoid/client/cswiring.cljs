(ns pintoid.client.cswiring
  (:use
   [pintoid.client.engine :only [update-world-snapshot!]]
   [pintoid.client.utils :only [panic! limit-str]])
  (:require
   [chord.client :refer [ws-ch]]
   [cljs.core.async :refer [<! >! close! timeout chan]]
   [weasel.repl :as repl])
  (:require-macros
   [taoensso.timbre :as timbre]
   [cljs.core.async.macros :refer [go go-loop]]))

(def client-server-time-diff 0)
(def ^:private timediff-history (atom nil))
(def ^:private ^:const timediff-history-size 100)
(def ^:private ^:const timediff-preserve 20)

(def ^:private server-ws-channel nil)
(def ^:private websocket-url (let [wl js/window.location] (str "ws://" (.-host wl) "/game-sock")))

(declare receive-server-messages)
(declare on-channel-connected)

(defn init-cs-communication []
  (go
    (timbre/info "Open websocket connection")
    (let [{:keys [ws-channel error]}
          (<! (ws-ch websocket-url {:format :transit-json
                                        :read-ch (chan 10)
                                        :write-ch (chan 10)}))]
      (if error
        (panic! error)
        (do
          (set! server-ws-channel ws-channel)
          (receive-server-messages ws-channel)
          (on-channel-connected))))))

(defn send-message-to-server [msg]
  (when-let [c server-ws-channel]
    (go (>! c msg))))

(declare receive-message)

(defn receive-server-messages [ws-chan]
  (go-loop []
    (when-let [{:keys [message error]} (<! ws-chan)]
      (if error
        (panic! error)
        (do
          (receive-message message)
          (recur))))))

(defn spawn-user-input-sender [make-user-input-snapshot-fn period]
  (timbre/debug "Spawn user input reading loop")
  (go-loop []
    (let [ui (make-user-input-snapshot-fn)]
      (send-message-to-server {:command :user-input :data ui}))
    (<! (timeout period))
    (recur)))

(defmulti receive-message (comp keyword :command))

(defn on-channel-connected []
  (timbre/trace "Send :join-game message")
  (send-message-to-server {:command :join-game}))

(defmethod receive-message :default [msg]
  (timbre/info "Unknown server message" msg))

(defn- update-clint-server-time-diff [time]
  (let [d1 (- time (js/performance.now))]
    (swap! timediff-history #(cons d1 (take timediff-history-size %))
           (let [d0 client-server-time-diff
                 tl @timediff-history
                 ls (sort (concat tl (repeat timediff-preserve d0)))
                 n (int (/ (count ls) 2))
                 v (nth ls n)]
             (when (not= client-server-time-diff v)
               (timbre/debugf "Update client-server-time-diff to %f" v))
             (set! client-server-time-diff v)
             ))))

(defmethod receive-message :wpatch [m]
  (update-clint-server-time-diff (:server-time m))
  (update-world-snapshot! m))

(defmethod receive-message :repl [m]
  (let [ws-url (:ws-url m)]
    (if (repl/alive?)
      (timbre/warnf "We already have active repl - not able to connect to " ws-url)
      (do
        (timbre/infof "Connecting to repl %s..." ws-url)
        (repl/connect ws-url)))))
