(ns pintoid.main
  (:require
   [pintoid.server.core :refer [start]])
  (:gen-class))

(defn -main
  ([] (pintoid.server.core/start))
  ([config] (pintoid.server.core/start config)))
