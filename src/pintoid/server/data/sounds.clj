(ns pintoid.server.data.sounds
  (:use [pintoid.server.entity]))

(def max-user-hear-distance 1500)
(def ref-user-hear-distance 500)

(defn- sound [& kv]
  (apply hash-map
         :max-dist max-user-hear-distance
         :ref-dist ref-user-hear-distance
         kv))

(defassets bullet-bang-sounds :sound
  [x [1 2 3]]
  (sound
   :source (str "/snd/bullet_" x ".ogg")
   :volume 0.45
   :rate 0.5
   ))

(defasset engine-sound :sound
  (sound
   :source (str "/snd/engine.ogg")
   :volume 0.7
   :loop true
   :rate 1.3
   ))
