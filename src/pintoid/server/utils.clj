(ns pintoid.server.utils)

(def enable-debug false)
(def eid-counter (atom 0))


(defn next-eid []
  (swap! eid-counter inc))


(defmacro log-info [& ms]
  `(println ~@ms))


(defmacro log-debug [& ms]
  (when enable-debug
    `(println ~@ms)))


(defn current-os-time []
  (System/currentTimeMillis))
