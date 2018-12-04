(ns pintoid.client.asset
  (:require
   [taoensso.timbre :as timbre :include-macros true])
  (:require-macros
   [pintoid.client.macros :refer [foreach!]]))

(def preload-all-assets false)

(defrecord Asset [name class proto obj deps])

(def assets (atom {}))
(def name-to-aids (atom {}))

(def ^:dynamic ^:private *used-assets* nil)
(def ^:dynamic ^:private *updated-assets* nil)

(defmulti load-asset (fn [aid proto] (:class proto)))
(defmulti unload-asset (fn [aid proto asset] (:class proto)))

(defn asset-id? [aid]
  (or (number? aid) (symbol? aid)))

(defmethod load-asset :default [aid proto]
  (timbre/warn "Unknown asset class" aid proto))

(defmethod unload-asset :default [aid proto asset]
  (timbre/debug "Unload asset %s" aid))

(defn- update-asset-obj! [aid obj deps]
  (when-let [ua *updated-assets*]
    (swap! ua conj aid))
  (when-let [a (@assets aid)]
    (swap! assets assoc aid (assoc a :obj obj :deps deps))))

(defn- maybe-resolve-name [n]
  (if (number? n)
    n
    (rand-nth (get @name-to-aids n))))

(defn track-used-assets [f & args]
  (let [deps (atom #{})]
    (binding [*used-assets* deps]
      (let [result (apply f args)]
        [result (set @deps)]))))

(defn- reload-asset-obj! [aid]
  (timbre/trace "Maybe reload asset" aid)
  (let [{:keys [obj proto]} (get @assets aid)]
    (when obj (unload-asset aid proto obj))
    (let [[obj deps] (track-used-assets load-asset aid proto)]
      (timbre/info "Load asset" aid (:name proto))
      (update-asset-obj! aid obj deps)
      obj)))

(defn- find-all-dependants [aid]
  (eduction
   (comp
    (filter #(-> % val :deps (contains? aid)))
    (map key))
   @assets))

(defn- clean-asset-obj-with-deps! [aid]
  (timbre/trace "Clean asset object" aid)
  (when-let [{:keys [proto obj] :as asset} (@assets aid)]
    (update-asset-obj! aid nil nil)
    (run! clean-asset-obj-with-deps! (find-all-dependants aid))
    (when obj
      (timbre/info "Unload asset" aid (:name proto))
      (unload-asset aid proto obj))))

(defn asset
  ([aid]
   (asset nil aid))
  ([class aid]
   (let [aid (maybe-resolve-name aid)
         a (get @assets aid)]
     (if a
       (do
         (when-let [x *used-assets*]
           (timbre/tracef "Depend on %s" aid)
           (swap! x conj aid))
         (when (and class (not= (:class a) class))
           (timbre/warnf "Asset %s has class %s, expected %s" aid (:class a) class))
         (if-let [obj (:obj a)]
           obj
           (reload-asset-obj! aid)))
       (do
         (timbre/warnf "Asset %s with class %s not found" aid class)
         nil)))))

(defn add-assets [ass]
  (timbre/infof "Add %d assets..." (count ass))
  (binding [*updated-assets* (atom #{})]
    (let [ids (map key ass)]
      (run! clean-asset-obj-with-deps! (reverse ids))
      (doseq [[aid proto] ass]
        (swap! *updated-assets* conj aid)
        (timbre/debug "Add asset" aid (:name proto))
        (swap! name-to-aids update (:name proto) (fnil conj []) aid)
        (swap! assets assoc aid (->Asset (:name proto) (:class proto) proto nil nil)))
      (timbre/info "Preload assets...")
      (doseq [[aid proto] ass]
        (when (or (:preload proto) preload-all-assets)
          (asset aid))))
    @*updated-assets*))

(defn add-asset [aid proto]
  (add-assets {aid proto}))

