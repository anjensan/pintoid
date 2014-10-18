(ns pintoid.server.core
  (:use [pintoid.server cs-comm game])
  (:require
   [pintoid.server.handler]
   [ring.middleware.params]
   [ring.middleware.session]
   [org.httpkit.server :refer [run-server]]))


(declare schedule-at-fixed-rate)


(defonce server (atom nil))

(defonce sched-executor
  (java.util.concurrent.ScheduledThreadPoolExecutor. 5))


(def app
  (-> #'pintoid.server.handler/app-routes
      (ring.middleware.params/wrap-params)
      (ring.middleware.session/wrap-session)))


(defn schedule-at-fixed-rate [period f]
  (.scheduleAtFixedRate
   sched-executor
   f 0 (long period)
   java.util.concurrent.TimeUnit/MILLISECONDS))


(defn stop-pintoid []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))


(defn start-pintoid []
  (reset! server (run-server #'app {:port 8080})))


(defn -main [& args]
  (start-pintoid))


;; FIXME
(init-world-state)
(schedule-at-fixed-rate 500 send-snapshots-to-all-clients)
(schedule-at-fixed-rate 50 run-world-simulation-tick)
