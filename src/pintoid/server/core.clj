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


(defmacro ^:private do-retfst [res & body]
  `(let [r# ~res] ~@body r#))


(defstate nrepl
  :start (let [ip (get-in config [:nrepl :bind] "127.0.0.1")
               port (get-in config [:nrepl :port] 9891)]
           (timbre/debug "Starting nrepl server...")
           (do-retfst
            (nrepl/start-server
             :bind ip :port port
             :init-ns 'pintoid.dev)
            (timbre/infof "Started nrepl server, port %s" port)))
  :stop (do-retfst
         (nrepl/stop-server nrepl)
         (timbre/debug "Stopped nrepl server")))


(defstate scheduler
  :start (let [th (get-in config [:scheduler :threads] 2)]
           (timbre/debug "Starting scheduled thread pool executor...")
           (do-retfst
            (ScheduledThreadPoolExecutor. th)
            (timbre/infof "Started scheduler, %s threads" th)))
  :stop (do-retfst
         (.shutdown scheduler)
         (timbre/infof "Stopped scheduler")))


(defn- sched-at-fixed-rate [scheduler f period]
  (.scheduleAtFixedRate
   scheduler
   f 0 (long period)
   java.util.concurrent.TimeUnit/MILLISECONDS))


(defstate tickers
  :start (let [ctick (get-in config [:scheduler :client-tickrate] 60)
               gtick (get-in config [:scheduler :game-tickrate] 30)]
           (do-retfst
            {:game (sched-at-fixed-rate
                    scheduler
                    #'game-world-tick
                    gtick)
             :client (sched-at-fixed-rate
                      scheduler
                      #'send-snapshots-to-all-clients
                      ctick)}
            (timbre/infof "Game tick %s, client tick %s" gtick ctick)))
  :stop (do-retfst
         (doseq [[_ t] tickers]
           (.cancel t true))
         (timbre/debug "Stopped tickers")))


(defn ring-handler []
   (-> #'pintoid.server.web/app-routes
       (ring.middleware.params/wrap-params)
       (ring.middleware.session/wrap-session)))


(defstate webserver
  :start (let [port (get-in config [:web :port] 8080)
               ip (get-in config [:web :bind] "0.0.0.0")]
           (timbre/debugf "Starting web server on port %s..." port)
           (do-retfst
            (hkit/run-server (ring-handler) {:ip ip :port port})
            (timbre/infof "Started web server, port %s" port)))
  :stop (do-retfst
         (webserver)
         (timbre/infof "Stopped web server")))


(defn start
  ([]
   (start
    (System/getenv "PINTOID_CONFIG")))
  ([cf]
   (->
    (mount/with-args {:config-file cf})
    (mount/start))))
