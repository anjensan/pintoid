(ns pintoid.server.core
  (:use [pintoid.server.cs-comm])
  (:require
   [pintoid.server.handler]
   [ring.middleware.params]
   [ring.middleware.session]
   [org.httpkit.server :refer [run-server]]))


(defonce server (atom nil))

(def app
  (-> #'pintoid.server.handler/app-routes
      (ring.middleware.params/wrap-params)
      (ring.middleware.session/wrap-session)))


(defn stop-pintoid []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))


(defn start-pintoid []
  (reset! server (run-server #'app {:port 8080})))


(defn -main [& args]
  (start-pintoid))


;;; FIXME: noooo
(spawn-players-notifier-loop)
