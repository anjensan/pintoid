(ns pintoid.server.data.core
  (:use [pintoid.server.ecs entity]))

(defn load-game-entities [w]
  (-> w
      (load-entities-from-ns 'pintoid.server.data.assets)
      (load-entities-from-ns 'pintoid.server.data.sounds)
      (load-entities-from-ns 'pintoid.server.data.starsky)
      (load-entities-from-ns 'pintoid.server.data.gameworld)))
