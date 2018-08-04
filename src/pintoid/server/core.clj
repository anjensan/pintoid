(ns pintoid.server.core
  (:use
   [pintoid.server.game.core]
   [pintoid.server cswiring web])
  (:require
   [mount.core :as mount :refer [defstate]]
   [aero.core :as aero]
   [taoensso.timbre :as timbre]
   [clojure.tools.nrepl.server :as nrepl]
   [org.httpkit.server :as hkit]
   [ring.middleware.params]
   [ring.middleware.session])
  (:import
   [java.util.concurrent TimeUnit ScheduledThreadPoolExecutor]))


(defstate config
  :start (if-let [cf (:config-file (mount/args))]
           (do
             (timbre/info "Load configuration file" cf)
             (aero/read-config cf))
           (timbre/warn "No configuration file provided - start with all defaults")))


(defstate nrepl
  :start (let [ip (get-in config [:nrepl :bind] "127.0.0.1")
               port (get-in config [:nrepl :port] 9891)]
           (timbre/infof "Start nrepl server at port %s" port)
           (nrepl/start-server :bind ip :port port
                               :init-ns 'pintoid.dev))
  :stop (do
          (timbre/info "Stop nrepl server")
          (nrepl/stop-server nrepl)))


(defstate scheduler
  :start (let [th (get-in config [:scheduler :threads] 2)]
           (timbre/infof "Start scheduler with %s threads" th)
           (ScheduledThreadPoolExecutor. th))
  :stop (do
          (timbre/info "Stop scheduler")
          (.shutdown scheduler)))

(defn- sched-at-fixed-rate [f period]
  (.scheduleAtFixedRate scheduler f 0 (long period) java.util.concurrent.TimeUnit/MILLISECONDS))

(defstate game-timer
  :start (let [gtick (get-in config [:scheduler :game-tickrate] 30)]
           (timbre/infof "Start game timer, period %sms" gtick)
           (sched-at-fixed-rate #'game-world-tick gtick))
  :stop (do (timbre/info "Stop game timer")
            (.cancel game-timer true)))

(defstate client-timer
  :start (let [ctick (get-in config [:scheduler :client-tickrate] 60)]
           (timbre/infof "Start client timer, period %sms" ctick)
           (sched-at-fixed-rate #'send-snapshots-to-all-clients ctick))
  :stop (do (timbre/info "Stop client timer")
            (.cancel client-timer true)))


(defn ring-handler []
   (-> #'pintoid.server.web/app-routes
       (ring.middleware.params/wrap-params)
       (ring.middleware.session/wrap-session)))

(defstate webserver
  :start (let [port (get-in config [:web :port] 8080)
               ip (get-in config [:web :bind] "0.0.0.0")]
           (timbre/infof "Start web server at port %s" port)
           (hkit/run-server (ring-handler) {:ip ip :port port}))
  :stop (do
          (timbre/info "Stop web server")
          (webserver)))


(defn start
  ([]
   (start (System/getenv "PINTOID_CONFIG")))
  ([cf]
   (->
    (mount/with-args {:config-file cf})
    (mount/start))))
