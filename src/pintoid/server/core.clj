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
   [java.util.concurrent
    TimeUnit
    ScheduledThreadPoolExecutor
    ScheduledExecutorService]))

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
  [{:keys [threads] :or {threads 1}}]
  (timbre/infof "Start scheduler" threads)
  (ScheduledThreadPoolExecutor. threads))

(defn stop-scheduler [^ScheduledThreadPoolExecutor s]
  (timbre/info "Stop scheduler")
  (.shutdown s))

(defn start-webserver
  [{:keys [bind port] :or {bind "0.0.0.0" port 8080}}]
  (timbre/infof "Start web server at port %s" port)
  (hkit/run-server (ring-handler) {:ip bind :port port}))

(defn stop-webserver [w]
  (timbre/info "Stop web server")
  (w))

(defn init-profiling
  [{:keys [enabled level ns-pattern]
    :or {enabled false level 2 ns-pattern "*"}}]
  (if enabled
    (do
      (timbre/infof "Initialize profiling at level %s, ns '%s'" level ns-pattern)
      (tufte/set-min-level! level)
      (tufte/set-ns-pattern! ns-pattern)
      true)
    (do
      (timbre/debug "Disable profiling")
      (tufte/set-min-level! 6)
      false)))

(defn start-pstats-aggregator []
  (timbre/info "Start pstats aggregator")
  (let [a (agent nil)]
    (tufte/add-handler! ::pstats-aggr #(send a tufte/merge-pstats (:pstats %)))
    a))

(defn stop-pstats-aggregator [a]
  (timbre/info "Stop pstats aggregator")
  (tufte/remove-handler! ::pstats-aggr)
  (send a (constantly nil)))

(defstate nrepl
  :start (start-nrepl (get (mount/args) :nrepl))
  :stop (stop-nrepl nrepl))

(defstate scheduler
  :start (start-scheduler (:scheduler (mount/args)))
  :stop (stop-scheduler scheduler))

(defn- sched-fixed-rate [name rate task]
  (timbre/infof "Start %s timer (%sms)" name rate)
  (let [pns (long rate)
        cnt (atom 2)
        done #(swap! cnt inc)
        f (fn []
            (if (<= @cnt 0)
              (timbre/debugf "Skip %s timer" name)
              (do
                (swap! cnt dec)
                (task done))))]
    (.scheduleAtFixedRate scheduler f pns pns TimeUnit/MILLISECONDS)))

(defn- unschedule [name timer]
  (timbre/info "Stop %s timer" name)
  (.cancel timer true))

(defstate game-timer
  :start (sched-fixed-rate "game"
          (get-in (mount/args) [:scheduler :game-tickrate] 30)
          #'game-world-tick)
  :stop (unschedule "game" game-timer))

(defstate client-timer
  :start (sched-fixed-rate "client"
          (get-in (mount/args) [:scheduler :client-tickrate] 30)
          #'send-snapshots-to-all-clients)
  :stop (unschedule "client" client-timer))

(defstate webserver
  :start (start-webserver (get (mount/args) :web))
  :stop (stop-webserver webserver))

(defstate profiling
  :start (init-profiling (get (mount/args) :profile)))

(defstate pstats-aggregator
  :start (when profiling (start-pstats-aggregator))
  :stop (when profiling (stop-pstats-aggregator pstats-aggregator)))

(defn- dump-n-clean-pstats [done]
  (send pstats-aggregator
        #(when % (timbre/info "Profile\n" (tufte/format-pstats %))))
  (done))

(defstate pstats-dump-timer
  :start (when profiling
           (sched-fixed-rate "profile"
            (get-in (mount/args) [:profile :period] 60000)
            #'dump-n-clean-pstats))
  :stop (when profiling (unschedule "profile" pstats-dump-timer)))

(defn start-config [c]
  (timbre/merge-config! (get c :log))
  (->
   (mount/with-args c)
   (mount/start)))

(defn start
  ([]
   (start (or (System/getenv "PINTOID_CONFIG") "config.edn")))
  ([cf]
   (start-config (load-config cf))))
