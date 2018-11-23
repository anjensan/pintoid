(ns pintoid.server.sound
  (:use
   [pintoid.ecs core system])
  (:require
   [taoensso.timbre :as timbre]
   [pintoid.ecs.core :refer :all]
   ))

(def default-sound-duration (* 20 1000))
(def ^:private sound-id-counter (atom 0))

(defn asys-garbage-sounds [w now]
  (timbre/tracef "Garbage collect sounds")
  (let [df (memoize (fn [sound-id]
                      (if (nil? sound-id)
                        default-sound-duration
                        (get (get-comp w sound-id :asset)
                             :duration
                             default-sound-duration))))]
    (combine-systems!
     (each-entity w eid [u ::until]
       (when-not (every? #(and (some? %)
                               (> % now))
                         (vals u))
         (let [s (get-comp w eid :sound)
               u' (into {}
                        (comp
                         (filter (fn [[k v]] (contains? s k)))
                         (map (fn [[k v :as kv]]
                                (if (nil? v)
                                  [k (+ now (df (get s k)))]
                                  kv)))
                         (filter (fn [[k v]] (> v now))))
                        u)
               s' (select-keys s (keys u'))]
           (fn->
            (put-comp! eid :sound s')
            (put-comp! eid ::until u'))))))))

(defn stop-all-sounds [w eid]
  (timbre/tracef "Stop all sounds for %s" eid)
  (->
   (drop-comp w eid :sound)
   (drop-comp w eid ::until)))

(defn stop-all-sounds! [w eid]
  (timbre/tracef "Stop all sounds for %s" eid)
  (->
   (drop-comp w eid :sound)
   (drop-comp w eid ::until)))

(defn play-sound
  ([w eid sound-id]
   (play-sound w eid (swap! sound-id-counter inc) sound-id))
  ([w eid play-id sound-id]
   (timbre/tracef "Play sound %s with key %s for %s" sound-id play-id eid)
   (-> w
       (update-comp eid :sound assoc play-id sound-id)
       (update-comp eid ::until assoc play-id nil))))

(defn play-sound!
  ([w eid sound-id]
   (play-sound! w eid (swap! sound-id-counter inc) sound-id))
  ([w eid play-id sound-id]
   (timbre/tracef "Play sound %s with key %s for %s" sound-id play-id eid)
   (-> w
       (update-comp! eid :sound assoc play-id sound-id)
       (update-comp! eid ::until assoc play-id nil))))

(defn stop-sound! [w eid play-id]
  (timbre/tracef "Stop play %s for %s" play-id eid)
  (-> w
      (update-comp! eid :sound dissoc play-id)
      (update-comp! eid ::until dissoc play-id)))

(defn stop-sound [w eid play-id]
  (timbre/tracef "Stop play %s for %s" play-id eid)
  (-> w
      (update-comp eid :sound dissoc play-id)
      (update-comp eid ::until dissoc play-id)))
