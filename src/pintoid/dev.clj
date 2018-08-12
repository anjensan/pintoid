(ns pintoid.dev
  (:require
   [clojure.pprint :as pp]
   [clojure.set :refer [union]]
   [pintoid.server.cswiring :refer [avatars send-to-client]]
   [pintoid.server.game.core :refer [world]]
   [pintoid.server.ecs :as ecs]
   [weasel.repl.websocket]
   [cemerick.piggieback])
  (:import
   java.net.InetAddress))


(defn get-players []
  (for [[eid ava] @avatars]
    {:pid eid :host (:host ava)}))

(defn get-entity [eid]
  (assoc (ecs/get-full-entity @world eid) :eid eid))

(defn get-entities
  ([q]
   (get-entities q (constantly true)))
  ([q f]
   (let [w @world]
     (->>
      (for [eid (ecs/entities w q)]
        (assoc (ecs/get-full-entity w eid) :eid eid))
      (filter f)))))


(defn avatar [pid]
  (pp/pprint (get @avatars pid)))

(defn entity [eid]
  (pp/pprint (get-entity eid)))

(defn entities
  ([q]
   (entities q (constantly true)))
  ([q f]
   (let [es (get-entities q f)
         ks (transduce (map (comp set keys)) union #{} es)]
     (pp/print-table (sort ks) es))))

(defn players []
  (pp/print-table (get-players)))


(defn- find-asset-world [w aid]
  (->> (ecs/entities w :assets)
       (map #(get (w % :assets) aid))
       (keep identity)
       (last)))

(defn asset [aid]
  (find-asset-world @world aid))


(defn- find-dev-asset-ids-world [w aid]
  (->> (ecs/entities w [:* :assets ::dev-asset])
       (filter #(contains? (w % :assets) aid))))


(defn reset-asset! [aid]
  (send world #(reduce ecs/remove-entity % (find-dev-asset-ids-world % aid)))
  (await world)
  (asset aid))

(defn update-asset! [aid f & args]
  (assert (apply f (asset aid) args))
  (send world
        (fn [w]
          (let [asset (find-asset-world w aid)
                asset' (apply f asset args)
                eid (last (find-dev-asset-ids-world w aid))
                e {:assets {aid asset'} ::dev-asset true}]
            (if eid
              (ecs/add-entity w eid e)
              (ecs/add-new-entity w e)))))
  (await world)
  (asset aid))


;; TODO: update-entity, add-entity, etc

(defn- localhost-address []
  (.getHostAddress (java.net.InetAddress/getLocalHost)))


(defn client-repl [pid & {:keys [ip port] :as opts}]
  (let [port (or port 9001)
        ip (or ip (localhost-address))
        ws-url (str "ws://" ip ":" port)
        avatar (get @avatars pid)
        connect-client #(send-to-client pid {:command :repl :ws-url ws-url})
        opts (assoc opts
                    :ip ip :port port
                    :pre-connect connect-client)]
    (if avatar
      (cemerick.piggieback/cljs-repl
       (apply weasel.repl.websocket/repl-env (mapcat identity opts)))
      (println "No user with pid" pid))))
