(ns pintoid.dev
  (:require
   [clojure.pprint :as pp]
   [clojure.set :refer [union]]
   [pintoid.server.cswiring :refer [avatars send-to-client]]
   [pintoid.server.game :refer [world dev-asystems]]
   [pintoid.ecs.core :as ecs]
   [weasel.repl.websocket]
   [cider.piggieback]
   [pintoid.server.collide]
   )
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

(defn print-entities
  ([q]
   (print-entities q (constantly true)))
  ([q f]
   (let [es (get-entities q f)
         ks (transduce (map (comp set keys)) union #{} es)]
     (pp/print-table (sort ks) es))))

(defn players []
  (pp/print-table (get-players)))

(defn update-protos
  ([] (update-protos true))
  ([t] (if t
        (swap! dev-asystems assoc :update-protos #'pintoid.ecs.entity/asys-actualize-entity-protos)
        (swap! dev-asystems dissoc :update-protos))))

(defn show-mbrs
  ([] (show-mbrs true))
  ([t] (swap! dev-asystems assoc :show-mbr #(pintoid.server.collide/asys-show-collision-mbrs % t))))

(defn into-world [vs]
  (send world into vs))

(defn send-world [& rs]
  (apply send world rs))

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
      (cider.piggieback/cljs-repl
       (apply weasel.repl.websocket/repl-env (mapcat identity opts)))
      (println "No user with pid" pid))))
