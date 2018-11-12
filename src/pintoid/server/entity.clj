(ns pintoid.server.entity
  (:require [taoensso.timbre :as timbre])
  (:use [pintoid.server.ecs core system]))

(defrecord ProtoInfo [var val args last])

(defn add-proto-info [cm var args]
  (assoc cm ::proto-info (->ProtoInfo var @var args cm)))

(defmacro defproto [name args & body]
  (timbre/debugf "Define proto '%s" name)
  `(defn ~name [& rs#]
     (apply
      (fn ~args (add-proto-info (do ~@body) (var ~name) rs#))
      rs#)))

(defn actualize-entity-proto [w e]
  (let-entity w e [{v :var f :val :as p} ::proto-info]
    (if (identical? @v f)
      w
      (let [{a :args c :last} p
            f' @v
            c' (apply f' a)
            cs (into {} (filter (fn [[k v]] (and (= (get c k) (get-comp w e k))
                                                 (not= v (get c k))))) c')
            pi (assoc p :val f' :last c')
            cs' (assoc cs ::proto-info pi)]
        (reduce (fn [w' [k v]] (put-comp w' e k v)) w cs')))))

(defn asys-actualize-entity-protos [w]
  (combine-systems
   (each-entity w e [{v :var f :val} ::proto-info]
     (when-not (identical? @v f)
       (timbre/debugf "Actualize proto for %s" e)
       #(actualize-entity-proto % e)))))

(defn- emit-entity-name [n]
  (list 'quote (symbol (name (ns-name *ns*)) (name n))))

(defmacro defentity
  [name spec]
  (timbre/debugf "Define entity '%s" name)
  (let [pn (symbol (str name "-proto"))]
    `(do
       (defonce ~name (next-entity))
       (defproto ~pn [] (assoc ~spec ::name ~(emit-entity-name name)))
       (alter-meta! (var ~name) assoc ::proto (var ~pn))
       ~name)))

(defmacro defentities
  [name bvec spec]
  (timbre/debugf "Define entities '%s" name)
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
  (timbre/debugf "Load entities from ns %s" ns)
  (when (and (symbol? ns) (nil? (find-ns ns))) (require ns))
  (reduce maybe-add-entity w (->> ns ns-map vals (mapcat eid-proto-from-entity-var))))

(defn load-entity-from-var [w v]
  (timbre/debugf "Load entity from var %s" v)
  (reduce maybe-add-entity w (or (eid-proto-from-entity-var v)
                                 (throw (ex-info "Invalid entity var" {:var v})))))

(defn unload-undefined-entities [w]
  (timbre/debugf "Unload undefined entities")
  (reduce
   (fn [w' e]
     (let-entity w e [n ::name]
       (if (resolve n) w' (remove-entity w' e))))
   w
   (entities w ::name)))

(defmacro defasset [name cls spec]
  (timbre/debugf "Define asset %s %s" name cls)
  `(defentity ~name
     {:asset (assoc ~spec :class ~cls :name ~(emit-entity-name name))}))

(defmacro defassets [name cls bvec spec]
  (timbre/debugf "Define assets %s %s" name cls)
  `(defentities ~name ~bvec
     {:asset (assoc ~spec :class ~cls :name ~(emit-entity-name name))}))
