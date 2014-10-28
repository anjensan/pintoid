(ns pintoid.server.core
  (:use [pintoid.server cs-comm game])
  (:require
   [pintoid.server.handler]
   [frodo.web :refer [App]]
   [ring.middleware.params]
   [ring.middleware.session]))


(defn schedule-at-fixed-rate [scheduler period f]
  (.scheduleAtFixedRate
   scheduler
   f 0 (long period)
   java.util.concurrent.TimeUnit/MILLISECONDS))

(defn start-scheduler! []
  (let [s (java.util.concurrent.ScheduledThreadPoolExecutor. 5)]
    (init-world-state)
    (schedule-at-fixed-rate s 80 send-snapshots-to-all-clients)
    (schedule-at-fixed-rate s 30 run-world-simulation-tick)
    s))

(defn stop-scheduler! [s]
  (.shutdown s))

(def app
  (reify
    App
    (start! [_]
      {:scheduler (start-scheduler!)
       :frodo/handler (-> #'pintoid.server.handler/app-routes
                          (ring.middleware.params/wrap-params)
                          (ring.middleware.session/wrap-session))})
    (stop! [_ s]
      (stop-scheduler! (:scheduler s)))))
