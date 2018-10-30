(ns pintoid.server.ecs.entity
  (:use [pintoid.server.ecs core]))

(defrecord ProtoInfo [var val args last])

(defn add-proto-info [cm var args]
  (assoc cm ::proto-info (->ProtoInfo var @var args cm)))

(defmacro defproto [name args & body]
  `(defn ~name [& rs#]
     (apply
      (fn ~args (add-proto-info (do ~@body) (var ~name) rs#))
      rs#)))

(defn maybe-actualize-entity-proto [w e]
  (let-entity w e [{v :var f :val :as p} ::proto-info]
    (if-not (identical? @v f)
      (let [{a :args c :last} p
            f' @v
            c' (apply f' a)]
        (-> w
            (into (comp
                    (filter (fn [[k v]] (= (get c k) (get-comp w e k))))
                    (filter (fn [[k v]] (not= v (get c k))))
                    (map (fn [[k v]] [e k v])))
                  c')
            (put-comp e ::proto-info (assoc p :val f' :last c'))))
      w)))

(defn actualize-entity-protos [w]
  (reduce maybe-actualize-entity-proto w (entities w ::proto)))

(defn- emit-entity-name [n]
  (list 'quote (symbol (name (ns-name *ns*)) (name n))))

(defmacro defentity
  [name spec]
  (let [pn (symbol (str name "-proto"))]
    `(do
       (defonce ~name (next-entity))
       (defproto ~pn [] (assoc ~spec ::name ~(emit-entity-name name)))
       (alter-meta! (var ~name) assoc ::proto (var ~pn))
       ~name)))

(defmacro defentities
  [name bvec spec]
  (let [pn (symbol (str name "-proto"))
        pg (symbol (str name "-eidgen"))
        pvec (vec (take-nth 2 bvec))]
    `(let [pa# (for ~bvec ~pvec)]
       (defonce ~pg (repeatedly next-entity))
       (defproto ~pn ~pvec (assoc ~spec ::name ~(emit-entity-name name)))
       (def ~name (vec (take (count pa#) ~pg)))
       (alter-meta! (var ~name) assoc ::proto (var ~pn) ::proto-args pa#)
       ~name)))

(defn- eid-proto-from-entity-var [v]
  (let [m (meta v)
        p (::proto m)]
    (when p
      (let [pa (::proto-args m [[]])
            vv @v]
        (map vector
             (if (seqable? vv) vv [vv])
             (map (fn [a] #(apply p a)) pa))))))

(defn- maybe-add-entity [w [e p]]
  (if-not (has-entity? w e) (add-entity w e (p)) w))

(defn load-entities-from-ns [w ns]
  (when (and (symbol? ns) (nil? (find-ns ns))) (require ns))
  (reduce maybe-add-entity w (->> ns ns-map vals (mapcat eid-proto-from-entity-var))))

(defn load-entity-from-var [w v]
  (reduce maybe-add-entity w (or (eid-proto-from-entity-var v)
                                 (throw (ex-info "Invalid entity var" {:var v})))))

(defn unload-undefined-entities [w]
  (entities-reduce w ::name
   (fn [w' e]
     (let-entity w e [n ::name]
       (if (resolve n) w' (remove-entity w' e))))))

(defmacro defasset [name cls spec]
  `(defentity ~name
     {:asset (assoc ~spec :class ~cls :name ~(emit-entity-name name))}))

(defmacro defassets [name cls bvec spec]
  `(defentities ~name ~bvec
     {:asset (assoc ~spec :class ~cls :name ~(emit-entity-name name))}))
