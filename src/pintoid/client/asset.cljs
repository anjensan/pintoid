(ns pintoid.client.asset
  (:require-macros
   [pintoid.client.macros :refer [foreach!]]
   [taoensso.timbre :as timbre]))

(def assets (atom))

(defmulti get-asset
  (fn [class id] class))


(defmulti load-asset
  (fn [id value] (:class value)))


(defmulti unload-asset
  (fn [id value] (:class value)))


(defmethod load-asset :default [id asset]
  (timbre/warn "Unknown asset class" (:class asset)))


(defmethod unload-asset :default [id asset]
  (timbre/debug "Unload asset %s" id))


(defmethod get-asset :default [class id]
  (timbre/warn "Unknown asset class" class)
  nil)


;; TODO: Add 'reload' support.
;; TODO: Build dependencies between assets.

(defn add-asset [id asset]
  (timbre/debug "Unload old asset" id)
  (when-let [a (get @assets id)]
    (unload-asset a))
  (let [c (:class asset)]
    (timbre/info "Load asset" c id)
    (swap! assets assoc id asset)
    (when asset
      (load-asset id asset))))
