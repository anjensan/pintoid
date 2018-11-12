(ns pintoid.client.graphics.core
  (:require
   [cljsjs.pixi]
   [pintoid.client.graphics.animation :as a]
   [pintoid.client.graphics.layer :as gl]
   [pintoid.client.graphics.sprite :as s]
   [pintoid.client.graphics.tilemap])
  (:require-macros
   [taoensso.timbre :as timbre]
   [pintoid.client.macros :refer [foreach!]]))

(def sprites (atom {}))
(def sprites-nil (atom {}))

(def pixi-layers nil)
(def pixi-renderer nil)

(defn- scale-canvas-to-window [c]
  (let [w js/window
        wiw (.-innerWidth w)
        wih (.-innerHeight w)
        cow (.-offsetWidth c)
        coh (.-offsetHeight c)
        sx (/ wiw cow)
        sy (/ wih coh)
        smm (/ (min sx sy) (max sx sy))
        ;; when sx ~ sy use max to avoid white borders around the canvas
        s (if (> smm 0.997) (max sx sy) (min sx sy))]
    (set! (.. c -style -transformOrigin) "0 0")
    (set! (.. c -style -transform) (str "scale(" s ")"))
    (set! (.. c -style -paddingLeft) 0)
    (set! (.. c -style -paddingRight) 0)
    (set! (.. c -style -paddingTop) 0)
    (set! (.. c -style -paddingBottom) 0)
    (set! (.. c -display) "block")
    (let [margin (str (/ (- wiw (* s cow)) 2) "px")]
      (set! (.. c -style -marginLeft) margin)
      (set! (.. c -style -marginRight) margin))
    (let [margin (str (/ (- wih (* s coh)) 2) "px")]
      (set! (.. c -style -marginTop) margin)
      (set! (.. c -style -marginBottom) margin))))

(defn init-autoscaling [canvas]
  (timbre/info "Enable canvas autoscaling")
  (.addEventListener
   js/window "resize"
   (fn [event]
     (when pixi-renderer
       (scale-canvas-to-window canvas))))
  (scale-canvas-to-window canvas))

(defn init-pixi-renderer [width height]
  (timbre/infof "Init pixi renderer, size [%s %s]" width height)
  (set! pixi-renderer (js/PIXI.autoDetectRenderer width height #js {:antialias true}))
  (set! pixi-layers (gl/init-layers-container width height))
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
  (when obj
    (when-let [p (.-parent obj)]
      (.removeChild p obj))
    (.destroy obj)))

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
  ([eid sprite props]
   (new-sprite eid nil sprite props))
  ([eid sid sprite props]
   (timbre/trace "Create sprite" sprite "for" [eid sid])
   (when-let [old-obj (get-sprite eid sid)]
     (.removeChild (.-parent old-obj) old-obj))
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
