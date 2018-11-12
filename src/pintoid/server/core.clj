(ns pintoid.server.core
  (:use
   [pintoid.server.game.core]
   [pintoid.server cswiring web])
  (:require
   [mount.core :as mount :refer [defstate]]
   [aero.core :as aero]
   [taoensso.timbre :as timbre]
   [taoensso.tufte :as tufte]
   [clojure.tools.nrepl.server :as nrepl]
   [org.httpkit.server :as hkit])
  (:import
   [java.util.concurrent TimeUnit ScheduledThreadPoolExecutor]))

(defn load-config [cf]
  (timbre/infof "Load config file '%s'" cf)
  (aero/read-config cf))

(defn start-nrepl
  [{:keys [enabled bind port]
    :or {enabled false bind "127.0.0.1" port 9891}}]
  (when enabled
    (timbre/infof "Start nrepl server at port %s" port)
    (nrepl/start-server :bind bind :port port :init-ns 'pintoid.dev)))

(defn stop-nrepl [s]
  (when s
    (timbre/infof "Stop nrepl server")
    (nrepl/stop-server s)))

(defn start-scheduler
  [{:keys [threads] :or {threads 4}}]
  (timbre/infof "Start scheduler with %s threads" threads)
  (ScheduledThreadPoolExecutor. threads))

(defn stop-scheduler [^ScheduledThreadPoolExecutor s]
  (timbre/info "Stop scheduler")
  (.shutdown s))

(defn sched-fixed-rate [s ^ScheduledThreadPoolExecutor n f p]
  (timbre/infof "Start %s timer (%sms)" n p)
  (.scheduleAtFixedRate
   s f 0 (long p)
   java.util.concurrent.TimeUnit/MILLISECONDS))

(defn unschedule [n t]
  (timbre/info "Stop %s timer" n)
  (.cancel t true))

(defn start-webserver
  [{:keys [bind port] :or {bind "0.0.0.0" port 8080}}]
  (timbre/infof "Start web server at port %s" port)
  (hkit/run-server (ring-handler) {:ip bind :port port}))

(defn stop-webserver [w]
  (timbre/info "Stop web server")
  (w))

(defn init-profiling
  [{:keys [enabled level ns-pattern] :or {enabled false}}]
  (if enabled
    (do
      (timbre/info "Initialize profiling")
      (tufte/set-min-level! level)
      (tufte/set-ns-pattern! ns-pattern)
      true)
    (do
      (timbre/debug "Disable profiling")
      (tufte/set-min-level! 6)
      false)))

(defn init-logging
  [c]
  (timbre/merge-config! c)
  :configured)

(defn start-pstats-aggregator []
  (timbre/info "Start pstats aggregator")
  (let [a (agent nil)]
    (tufte/add-handler! ::pstats-aggr #(send a tufte/merge-pstats (:pstats %)))
    a))

(defn stop-pstats-aggregator [a]
  (timbre/info "Stop pstats aggregator")
  (tufte/remove-handler! ::pstats-aggr)
  (send a (constantly nil)))


(defstate config
  :start (load-config (:config-file (mount/args))))

(defstate logging
  :start (init-logging (get config :log)))

(defstate nrepl
  :start (start-nrepl (get config :nrepl))
  :stop (stop-nrepl nrepl))

(defstate scheduler
  :start (start-scheduler (:scheduler config))
  :stop (stop-scheduler scheduler))

(defstate game-timer
  :start (sched-fixed-rate
          scheduler "game" #'game-world-tick
          (get-in config [:scheduler :game-tickrate] 30))
  :stop (unschedule "game" game-timer))

(defstate client-timer
  :start (sched-fixed-rate scheduler "client" #'send-snapshots-to-all-clients
          (get-in config [:scheduler :client-tickrate] 60))
  :stop (unschedule "client" client-timer))

(defstate webserver
  :start (start-webserver (get config :web))
  :stop (stop-webserver webserver))

(defstate profiling
  :start (init-profiling (get config :profile)))

(defstate pstats-aggregator
  :start (when profiling (start-pstats-aggregator))
  :stop (when profiling (stop-pstats-aggregator pstats-aggregator)))

(defn- dump-n-clean-pstats []
  (send pstats-aggregator
   #(when % (timbre/info "Profile\n" (tufte/format-pstats %)))))

(defstate pstats-dump-timer
  :start (when profiling
           (sched-fixed-rate scheduler "profile" #'dump-n-clean-pstats
            (get-in config [:profile :period] 60000)))
  :stop (when profiling (unschedule "profile" pstats-dump-timer)))

(defn start
  ([]
   (start (or (System/getenv "PINTOID_CONFIG") "config.edn")))
  ([cf]
   (->
    (mount/with-args {:config-file cf})
    (mount/start))))
