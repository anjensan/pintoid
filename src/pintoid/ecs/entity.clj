(ns pintoid.ecs.entity
  (:require [taoensso.timbre :as timbre])
  (:use [pintoid.ecs core system]))

(defrecord ProtoInfo [deps var args last])

(defn add-proto-info [cm var args]
  (assoc cm ::proto-info
         (->ProtoInfo (cons [var @var] (-> cm ::proto-info :deps))
                      var args cm)))

(defmacro defproto [name args & body]
  `(defn ~name [& rs#]
     (apply
      (fn ~args (add-proto-info (do ~@body) (var ~name) rs#))
      rs#)))

(defn- proto-info-changed? [{:keys [deps]}]
  (some (fn [[a b]] (not (identical? @a b))) deps))

(defn actualize-entity-proto [w e]
  (let-entity w e [pi ::proto-info]
    (if-not (proto-info-changed? pi)
      w
      (let [deps' (mapv (fn [[a _]] [a @a]) (:deps pi))
            {args :args last :last} pi
            f' @(:var pi)
            new (apply f' args)
            cs (into {}
                (filter
                 (fn [[k v]]
                   (and (= (get last k) (get-comp w e k))
                        (not= v (get last k)))))
                (merge
                 (zipmap (keys last) (repeat nil))
                 new))
            pi (assoc pi :last new :deps deps')
            cs' (assoc cs ::proto-info pi)]
        (reduce (fn [w' [k v]] (put-comp w' e k v)) w cs')))))

(defn asys-actualize-entity-protos [w]
  (combine-systems
   (each-entity w e [pi ::proto-info]
     (when (proto-info-changed? pi)
       (timbre/debugf "Actualize proto for %s" e)
       #(actualize-entity-proto % e)))))

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
        pi (::proto m)]
    (when pi
      (let [pa (::proto-args m [[]])
            vv @v]
        (map vector
             (if (seqable? vv) vv [vv])
             (map (fn [a] #(apply pi a)) pa))))))

(defn- maybe-add-entity [w [e pi]]
  (if-not (has-entity? w e)
    (let [x (pi)]
      (if-let [a (:asset x)]
        (timbre/debugf "Add asset %s, type %s, name %s" e (:class a) (:name a))
        (timbre/debugf "Add entity %s, name %s" e (::name x)))
      (add-entity w e x))
    w))

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
  `(defentity ~name
     {:asset (assoc ~spec :class ~cls :name ~(emit-entity-name name))}))

(defmacro defassets [name cls bvec spec]
  `(defentities ~name ~bvec
     {:asset (assoc ~spec :class ~cls :name ~(emit-entity-name name))}))

(defmacro defconst [name value]
  (let [pn (symbol (str name "-asset"))]
    `(let [c# ~value]
       (def ~name ~value)
       (defentity ~pn {:asset {:value ~value, :class :const, :name ~(emit-entity-name name)}})
       (var ~name))))

