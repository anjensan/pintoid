(ns pintoid.client.graphics.layer
  (:require
   [cljsjs.pixi]
   [pintoid.client.asset :as as]
   [pintoid.client.graphics.utils :refer [->point ->pair]]
   [pintoid.client.graphics.animation :as a]
   [pintoid.client.graphics.animloop :as al]
   [taoensso.timbre :as timbre]))


(defonce lcontainer (js/PIXI.Container.))
(defonce layers (atom {}))
(defonce camera-size (atom [1 1]))
(defonce camera (atom {}))


(defn set-layer-properties [obj props]
  (when-let [pivot (get props :pivot)] (set! (.-pivot obj) (->point pivot)))
  (when-let [alpha (get props :alpha)] (set! (.-alpha obj) alpha))
  (when-let [visible (get props :visible)] (set! (.-visible obj) visible)))


(defn- coerce-layer-properties [layer]
  (assoc layer
         :parallax (->pair (:parallax layer 1))
         :scale-rate (->pair (:scale-rate layer 1))))


(defmethod as/load-asset :layer [id layer]
  (let [lo (js/PIXI.Container.)
        layer (coerce-layer-properties layer)]
    (set-layer-properties lo layer)
    (set! (.-zorder lo) (:zorder layer 0))
    (swap! layers assoc id (assoc layer :pixi-obj lo))
    (.addChild lcontainer lo)
    (.sort (.. lcontainer -children)
           #(compare (.-zorder %1) (.-zorder %2)))))


(defmethod as/unload-asset :layer [id layer]
  (when-let [lo (get-in @layers [id :pixi-obj])]
    (.removeChild lcontainer lo)
    (swap! layers dissoc id)))


(defmethod as/get-asset :layer [class id]
  (get @layers id))


(defn- get-layer-pixi-obj [lid]
  (let [ls @layers
        lid (or lid :default)
        lo (if (contains? ls lid)
             (get ls lid)
             (do
               (timbre/warnf "Unknown layer %s" lid)
               (get ls :default)))]
   (get lo :pixi-obj)))


(defn layer-add [layer-id obj]
  (when-let [po (get-layer-pixi-obj layer-id)]
    (.addChild po obj)))


(defn layer-remove [layer-id obj]
  (when-let [po (get-layer-pixi-obj layer-id)]
    (.removeChild po obj)))


(defn- set-viewport-for-layer!
  [{obj :pixi-obj [px py] :parallax [srx sry] :scale-rate} x y sx sy]
  (set! (.. obj -position -x) (-> px (* x) (-)))
  (set! (.. obj -position -y) (-> py (* y) (-)))
  (set! (.. obj -scale -x) (-> srx (/ sx) (+ 1) (- srx) (/)))
  (set! (.. obj -scale -x) (-> sry (/ sy) (+ 1) (- sry) (/))))


(defn set-viewport!
  ([x y s]
   (set-viewport! x y s s))
  ([x y sx sy]
   (doseq [[lid lo] @layers]
     (set-viewport-for-layer! lo x y sx sy))))


(defn get-sprite-view-rect [sprite]
  (loop [s sprite]
    (if-let [vr (.-viewrect s)]
      vr
      (when-let [p (.-parent s)] (recur p)))))


(defn init-layers-container [width height]
  (set! (.. lcontainer -position -x) (/ width 2))
  (set! (.. lcontainer -position -y) (/ height 2))
  (set! (.. lcontainer -viewrect) #js [[0 0] [width height]])
  (reset! camera-size (constantly [width height]))
  (as/add-asset :layer/default {:class :layer})
  lcontainer)
