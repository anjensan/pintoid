(ns pintoid.client.sound
  (:require
   [cljsjs.howler]
   [pintoid.client.asset :as as]
   [taoensso.timbre :as timbre])
  (:require-macros
   [taoensso.timbre :as timbre]
   [pintoid.client.macros :refer [foreach!]]))

(def sounds (atom {}))

(defmethod as/load-asset :sound [_ proto]
  (js/Howl.
   (clj->js
    {:src (:source proto)
     :rate (:rate proto 1)
     :volume (:volume proto 0.5)
     :loop (:loop proto false)
     :maxDistance (:max-dist proto)
     :refDistance (:ref-dist proto)
     :rolloffFactor (:rolloff proto 1)
     :preload true
     :distanceModel "linear"
     })))

(defmethod as/unload-asset :sound [id proto h]
  ;; TODO: recreate active @sounds
  (.unload h))

(defn set-listener-pos [[x y]]
  ;; TODO: Extern 'Howler.pos'
  ((.bind (aget js/Howler "pos") js/Howler) x y 0))

(defn set-sound-pos [eid [x y]]
  (doseq [[p h] (vals (get @sounds eid))]
    ;; TODO: Extern 'Howl.pos'
    ((.bind (aget h "pos") h) x y nil p)))

(defn- add-sound-p [eid sid obj]
  (swap! sounds assoc-in [eid sid] obj))

(defn- remove-sound-p [eid sid]
  (swap! sounds update eid dissoc sid)
  (swap! sounds #(if (seq (get % eid)) % (dissoc % eid))))

(defn stop-sound [eid sid]
  (timbre/tracef "Stop sound, entity %s, sid %s" eid sid)
  (when-let [[p h] (get-in @sounds [eid sid])]
    (.stop h p)))

(defn stop-sounds [eid]
  (timbre/tracef "Stop all sounds, entity %s" eid)
  (doseq [[sid [p h]] (get @sounds eid)]
    (.stop h p)))

(defn play-sound [eid sid sound]
  (timbre/tracef "Play sound, entity %s, sid %s, sound %s" eid sid sound)
  (when-let [[p h] (get-in @sounds [eid sid])]
    (.stop h p))
  (let [h (as/asset :sound sound)
        p (.play h)]
    (.once h "stop" #(remove-sound-p eid p))
    (add-sound-p eid sid [p h])))

