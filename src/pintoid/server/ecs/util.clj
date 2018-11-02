(ns pintoid.server.ecs.util
  (:require [clojure.data.int-map :as im])
  (:import [clojure.data.int_map PersistentIntMap]))

(defn transient? [c]
  (instance? clojure.lang.ITransientCollection c))

(defn maybe-transient [c]
  (if (transient? c) c (transient c)))

(defn maybe-persistent! [c]
  (if (transient? c) (persistent! c) c))

(defn transient-reduce [f i c]
  (persistent! (reduce f i (transient c))))

(defn assoc-dissoc-coll [c k v]
  (if (zero? (count v))
    (dissoc c k)
    (assoc c k v)))

(defn assoc-dissoc-coll! [c k v]
  (if (zero? (count v))
    (dissoc! c k)
    (assoc! c k v)))

(defn edcat [& cs]
  (eduction cat cs))

(defn to-int-map [m]
  (if (instance? PersistentIntMap m) m (into (im/int-map) m)))
