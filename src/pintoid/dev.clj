(ns pintoid.dev
  (:require
   [clojure.pprint :as pp]
   [clojure.set :refer [union]]
   [pintoid.server.cswiring :refer [avatars send-to-client]]
   [pintoid.server.game :refer [world dev-asystems time-speed]]
   [pintoid.ecs.core :as ecs]
   [pintoid.ecs.entity :as ecse]
   [weasel.repl.websocket]
   [pintoid.server.devtools :as dt]
   [cider.piggieback]
   )
  (:import
   java.net.InetAddress))

(defn get-players []
  (for [[eid ava] @avatars
        :let [a @ava]]
    (into
     (array-map :pid eid :host (:host a))
     (ecs/get-comp @world eid :player))))

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
  (pp/pprint (into {} (filter (comp simple-keyword? key) (get-entity eid)))))

(defn print-entities
  ([q]
   (print-entities q (constantly true)))
  ([q f]
   (let [es (get-entities q f)
         ks (transduce (map (comp set keys)) union #{} es)]
     (pp/print-table (sort (filter simple-keyword? ks)) es))))

(defn players []
  (pp/print-table (get-players)))

(defn auto-update-protos
  ([] (auto-update-protos true))
  ([t] (if t
        (swap! dev-asystems assoc ::update-protos #'ecse/asys-actualize-entity-protos)
        (swap! dev-asystems dissoc ::update-protos))
   t))

(defn- make-show-fn [k asys]
  (letfn [(show
            ([] (show true))
            ([t] (swap! dev-asystems assoc k #(asys % t)) t))]
    show))

(def show-mbr (make-show-fn ::mbr #'dt/asys-show-collision-mbrs))
(def show-vxy (make-show-fn ::vxy #'dt/asys-show-velocity))
(def show-fxy (make-show-fn ::fxy #'dt/asys-show-force))

(defn into-world [vs]
  (send world into vs))

(defn send-world [& rs]
  (apply send world rs))

(defn speed [s]
  (assert (<= 0 s 10))
  (reset! time-speed s))

(defn pause []
  (speed 0))

(defn resume []
  (speed 1))

(defn- localhost-address []
  (.getHostAddress (java.net.InetAddress/getLocalHost)))

(defn client-repl-connect
  [pid & {:keys [ip port] :as opts}]
  (let [port (or port 9001)
        ip (or ip (localhost-address))
        ws-url (str "ws://" ip ":" port)]
    (send-to-client pid {:command :repl :ws-url ws-url})
    nil))

(defn- find-single-pid []
  (let [as @avatars]
    (when (== 1 (count as)) (ffirst as))))

(defn client-repl
  [& {:keys [pid ip port] :as opts
      :or {pid (find-single-pid)
           ip (localhost-address)
           port 9001}}]
  (if-let [avatar (get @avatars pid)]
    (let [ws-url (str "ws://" ip ":" port)
          connect-client #(send-to-client pid {:command :repl :ws-url ws-url})
          opts (assoc opts
                      :ip ip
                      :port port
                      :pre-connect connect-client)
          env (apply weasel.repl.websocket/repl-env (mapcat identity opts))]
      (cider.piggieback/cljs-repl env))
    (println "No user with pid" pid)))

(do
  (auto-update-protos))
