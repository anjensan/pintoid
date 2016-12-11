(ns pintoid.client.asset
  (:require
   [taoensso.timbre :as timbre :include-macros true])
  (:require-macros
   [pintoid.client.macros :refer [foreach!]]))

(defrecord Asset [class proto obj deps])
(def assets (atom {}))

(def ^:dynamic ^:private *used-assets* nil)
(def ^:dynamic ^:private *updated-assets* nil)

(defn asset-class [aid proto]
  (or (:class proto) (keyword (namespace aid))))

(defmulti load-asset asset-class)
(defmulti unload-asset (fn [aid proto asset] (asset-class aid proto)))


(defmethod load-asset :default [aid proto]
  (timbre/warn "Unknown asset class" aid))

(defmethod unload-asset :default [aid proto asset]
  (timbre/debug "Unload asset %s" aid))


(defn- update-asset-obj! [aid obj deps]
  (when-let [ua *updated-assets*]
    (swap! ua conj aid))
  (when-let [a (@assets aid)]
    (swap! assets assoc aid (assoc a :obj obj :deps deps))))


(defn- get-asset [aid]
  (when-let [x *used-assets*]
    (timbre/debug "Depend on %s" aid)
    (swap! x conj aid))
  (@assets aid))


(defn track-used-assets [f & args]
  (let [deps (atom #{})]
    (binding [*used-assets* deps]
      (let [result (apply f args)]
        [result (set @deps)]))))


(defn- reload-asset-obj! [aid]
  (let [{:keys [obj proto]} (@assets aid)]
    (when obj
      (timbre/info "Unload old asset" aid)
      (unload-asset aid proto obj))
    (let [[obj deps] (track-used-assets load-asset aid proto)]
      (timbre/info "Load asset" aid)
      (update-asset-obj! aid obj deps)
      obj)))


(defn- find-all-dependants [aid]
  (eduction
   (comp
    (filter #(-> % val :deps (contains? aid)))
    (map key))
   @assets))


(defn- clean-asset-obj-with-deps! [aid]
  (when-let [{:keys [proto obj] :as asset} (@assets aid)]
    (update-asset-obj! aid nil nil)
    (run! clean-asset-obj-with-deps! (find-all-dependants aid))
    (when obj
      (timbre/info "Unload asset" aid)
      (unload-asset aid proto obj))))


(defn asset
  ([aid]
   (asset nil aid))
  ([class aid]
   (let [a (get-asset aid)]
     (when (and class (== (:class a) class))
       (timbre/warn "Asset %s has class %s, expected %s" aid (:class a) class))
     (if-let [obj (:obj a)]
       obj
       (reload-asset-obj! aid)))))


(defn add-assets [ass]
  (binding [*updated-assets* (atom #{})]
    (let [ass (sort-by (fn [[k v]] [(:load-order v) k]) ass)
          ass-ids (map key ass)]
      (run! clean-asset-obj-with-deps! (reverse ass-ids))
      (doseq [[aid proto] ass]
        (swap! *updated-assets* conj aid)
        (timbre/info "Add asset" aid)
        (swap! assets assoc aid
               (Asset. (asset-class aid proto) proto nil nil)))
      (timbre/debug "Preload assets...")
      (run! asset ass-ids))
    @*updated-assets*))


(defn add-asset [aid proto]
  (add-assets {aid proto}))

