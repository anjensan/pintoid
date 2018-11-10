(ns pintoid.server.game.sound
  (:use
   [pintoid.server.ecs core system entity])
  (:require
   [taoensso.timbre :as timbre]
   [pintoid.server.ecs.core :refer :all]
   ))

(def default-sound-duration (* 20 1000))
(def ^:private sound-id-counter (atom 0))

(defn asys-garbage-sounds [w now]
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
  (->
   (drop-comp w eid :sound)
   (drop-comp w eid ::until)))

(defn stop-all-sounds! [w eid]
  (->
   (drop-comp w eid :sound)
   (drop-comp w eid ::until)))

(defn play-sound
  ([w eid sound-id]
   (play-sound w eid (swap! sound-id-counter inc) sound-id))
  ([w eid play-id sound-id]
   (-> w
       (update-comp eid :sound assoc play-id sound-id)
       (update-comp eid ::until assoc play-id nil))))

(defn play-sound!
  ([w eid sound-id]
   (play-sound! w eid (swap! sound-id-counter inc) sound-id))
  ([w eid play-id sound-id]
   (-> w
       (update-comp! eid :sound assoc play-id sound-id)
       (update-comp! eid ::until assoc play-id nil))))

(defn stop-sound! [w eid play-id]
  (-> w
      (update-comp! eid :sound dissoc play-id)
      (update-comp! eid ::until dissoc play-id)))

(defn stop-sound [w eid play-id]
  (-> w
      (update-comp eid :sound dissoc play-id)
      (update-comp eid ::until dissoc play-id)))
