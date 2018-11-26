(ns pintoid.client.layer
  (:require
   [cljsjs.pixi]
   [pintoid.client.asset :as as]
   [pintoid.client.utils :refer [->point ->pair]]
   [pintoid.client.animation :as a]
   [pintoid.client.animloop :as al]
   [taoensso.timbre :as timbre]))

(defonce lcontainer (js/PIXI.Container.))
(defonce root-layer nil)

(defonce layers (atom #{}))
(defonce camera-size (atom [1 1]))
(defonce camera (atom {}))

(defn- set-layer-properties [obj props]
  (when-let [pivot (get props :pivot)] (set! (.-pivot obj) (->point pivot)))
  (when-let [alpha (get props :alpha)] (set! (.-alpha obj) alpha))
  (when-let [visible (get props :visible)] (set! (.-visible obj) visible)))

(defn- coerce-layer-properties [layer]
  (assoc layer
         :parallax (->pair (:parallax layer 1))
         :scale-rate (->pair (:scale-rate layer 1))))

(defn- reorder-layers []
  (.sort (.. lcontainer -children)
         #(compare (.-zorder %1) (.-zorder %2))))

(defmethod as/load-asset :layer [id layer]
  (let [lo (js/PIXI.Container.)
        layer (coerce-layer-properties layer)]
    (set-layer-properties lo layer)
    (set! (.-zorder lo) (:zorder layer 0))
    (swap! layers conj id)
    (.addChild lcontainer lo)
    (reorder-layers)
    (assoc layer :pixi-obj lo)))

(defn- init-root-layer []
  (let [lo (js/PIXI.Container.)
        p {:parallax [1 1] :scale-rate [1 1]}]
    (set-layer-properties lo p)
    (set! (.-zorder lo) 0)
    (set! root-layer (assoc p :pixi-obj lo))
    (swap! layers conj nil)
    (.addChild lcontainer lo)
    (reorder-layers)))

(defmethod as/unload-asset :layer [id proto lo]
  (.removeChild lcontainer lo)
  (reorder-layers))

(defn- get-layer-asset [lid]
  (if (nil? lid)
    root-layer
    (or (as/asset :layer lid) root-layer)))

(defn layer-add [layer-id obj]
  (when-let [po (:pixi-obj (get-layer-asset layer-id))]
    (.addChild po obj)))

(defn layer-remove [layer-id obj]
  (when-let [po (:pixi-obj (get-layer-asset layer-id))]
    (.removeChild po obj)))

(defn- set-viewport-for-layer!
  [{obj :pixi-obj [px py] :parallax [srx sry] :scale-rate} x y sx sy]
  (let [ssx (-> srx (/ sx) (+ 1) (- srx) (/))
        ssy (-> sry (/ sy) (+ 1) (- sry) (/))]
    (set! (.. obj -position -x) (-> px (* ssx) (* x) (-)))
    (set! (.. obj -position -y) (-> py (* ssy) (* y) (-)))
    (set! (.. obj -scale -x) ssx)
    (set! (.. obj -scale -y) ssy)))

(defn set-viewport!
  ([x y s]
   (set-viewport! x y s s))
  ([x y sx sy]
   (doseq [lid @layers]
     (set-viewport-for-layer! (get-layer-asset lid) x y sx sy))))

(defn get-sprite-view-rect [sprite]
  (loop [s sprite]
    (if-let [vr (.-viewrect s)]
      vr
      (when-let [p (.-parent s)] (recur p)))))

(defn resize-layers-container [width height scale]
  (set! (.. lcontainer -position -x) (/ width 2))
  (set! (.. lcontainer -position -y) (/ height 2))
  (set! (.. lcontainer -scale -x) scale)
  (set! (.. lcontainer -scale -y) scale)
  (set! (.. lcontainer -viewrect) #js [[0 0] [width height]])
  (reset! camera-size (constantly [width height])))

(defn init-layers-container []
  (init-root-layer)
  lcontainer)
