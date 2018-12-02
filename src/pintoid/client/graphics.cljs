(ns pintoid.client.graphics
  (:require
   [cljsjs.pixi]
   [pintoid.client.animation :as a]
   [pintoid.client.layer :as gl]
   [pintoid.client.sprite :as s]
   [pintoid.client.tilemap])
  (:require-macros
   [taoensso.timbre :as timbre]
   [pintoid.client.macros :refer [foreach!]]))

(def sprites (atom {}))
(def sprites-nil (atom {}))

(def pixi-layers nil)
(def pixi-renderer nil)
(def pixi-base-height nil)
(def pixi-base-width nil)

(defn- scale-canvas-to-window [_]
  (let [wiw (.-innerWidth js/window)
        wih (.-innerHeight js/window)
        sx (/ wiw pixi-base-width)
        sy (/ wih pixi-base-height)
        s (max sx sy)]
    (.resize pixi-renderer wiw wih)
    (gl/resize-layers-container wiw wih s)))

(defn init-pixi-renderer [width height]
  (timbre/infof "Init pixi renderer, size [%s %s]" width height)
  (set! pixi-base-width width)
  (set! pixi-base-height height)
  (set! pixi-renderer (js/PIXI.autoDetectRenderer width height #js {:antialias true :autoResize true}))
  (set! pixi-layers (gl/init-layers-container))
  (.addEventListener js/window "resize" scale-canvas-to-window)
  (scale-canvas-to-window nil)
  (.-view pixi-renderer))

(defn get-sprite
  ([eid]
   (get @sprites-nil eid))
  ([eid sid]
   (if (nil? sid)
     (get @sprites-nil eid)
     (get (get @sprites eid) sid))))

(defn find-sprites-by-eid [eid]
  (when-let [s (get @sprites eid)]
    (vals s)))

(defn- destroy-sprite [obj]
  (timbre/tracef "Destroy %s" obj)
  (.destroy obj #js {:children true}))

(defn remove-sprites-by-eid [eid]
  (when-let [m (get @sprites eid)]
    (doseq [s (vals m)]
      (destroy-sprite s))
    (swap! sprites dissoc eid)
    (swap! sprites-nil dissoc eid)))

(defn remove-sprite
  ([eid]
   (remove-sprite eid nil))
  ([eid sid]
   (let [ss @sprites
         m (get ss eid)]
     (when-let [s (get m sid)]
       (let [m' (dissoc m sid)]
         (if (empty m')
           (swap! sprites dissoc eid)
           (swap! sprites assoc eid m')))
       (destroy-sprite s)))))

(defn new-sprite
  ([eid sprite]
   (new-sprite eid nil sprite))
  ([eid sid sprite]
   (new-sprite eid sid sprite nil))
  ([eid sid sprite props]
   (timbre/tracef "Create sprite %s for %s" sprite [eid sid])
   (when-let [old-obj (get-sprite eid sid)]
     (destroy-sprite old-obj))
   (let [sprite-spec (s/get-sprite-spec sprite)
         layer-id (or (:layer props) (:layer sprite-spec))
         sprite (s/make-sprite sprite props)]
     (gl/layer-add layer-id sprite)
     (swap! sprites assoc-in [eid sid] sprite)
     (when (nil? sid)
       (swap! sprites-nil assoc eid sprite))
     sprite)))

(defn render-graphics! []
  (.render pixi-renderer pixi-layers))
