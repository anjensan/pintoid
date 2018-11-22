(ns pintoid.assets.core
  (:use [pintoid.server.entity]))

(defn load-game-entities [w]
  (-> w
      (load-entities-from-ns 'pintoid.assets.sprites)
      (load-entities-from-ns 'pintoid.assets.sounds)
      (load-entities-from-ns 'pintoid.assets.starsky)
      (load-entities-from-ns 'pintoid.assets.gameworld)))
