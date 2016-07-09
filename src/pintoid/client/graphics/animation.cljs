(ns pintoid.client.graphics.animation
  (:require
   [taoensso.timbre :as timbre :include-macros true]
   [pintoid.client.graphics.animloop :refer [add-animation!]]))


(def animation-uid-counter (atom 0))

(defn- object-animation-id [anim-kind obj]
  (let [aid (or
             (.--pintoid-anim-uid obj)
             (set! (.--pintoid-anim-uid obj)
                   (str (swap! animation-uid-counter inc))))]
    (.concat aid anim-kind)))


(defn- mk-linear-interpolator [t1 t2 v1 v2]
  (let [t2-t1 (- t2 t1)
        ddc (/ 1 t2-t1)]
    (fn [t]
      (let [dd (* (- t2 t) ddc)
            sd (- 1 dd)]
        (+ (* v1 dd) (* v2 sd))))))


(defn- anim-linear-updater
  [obj t1 t2 xy1 xy2]
  (let [p (.-position obj)
        [x1 y1] xy1
        [x2 y2] xy2
        xi (mk-linear-interpolator t1 t2 x1 x2)
        yi (mk-linear-interpolator t1 t2 y1 y2)]
    (fn [time]
      (let [x' (xi time)
            y' (yi time)]
        (set! (.-x p) x')
        (set! (.-y p) y')))))


(defn- anim-linear-finisher
  [obj xy2]
  (fn []
    (let [[x2 y2] xy2]
      (set! (.. obj -position -x) x2)
      (set! (.. obj -position -y) y2))))


(defn- anim-linear-rotate-updater [obj t1 t2 angle1 angle2]
  (let [ai (mk-linear-interpolator t1 t2 angle1 angle2)]
    (fn [t]
      (set! (.-rotation obj) (ai t)))))


(defn- anim-linear-rotate-finisher [obj angle]
  (fn []
    (set! (.-rotation obj) angle)))


(defn linear-move [obj t1 t2 xy1 xy2]
  (timbre/trace "linear-move" obj t1 t2 xy1 xy2)
  (add-animation!
   (object-animation-id obj "lm") t1 t2
   (when xy1 (anim-linear-updater obj t1 t2 xy1 xy2))
   nil
   (anim-linear-finisher obj xy2)))


(defn instant-move [obj t1 t2 xy]
  (timbre/trace "instant-move" obj t1 t2 xy)
  (add-animation!
   (object-animation-id obj "lm") t1 t2
   nil
   nil
   (anim-linear-finisher obj xy)))


(defn linear-rotate [obj t1 t2 angle1 angle2]
  (timbre/trace "linear-rotate" obj t1 t2 angle1 angle2)
  (add-animation!
   (object-animation-id obj "rot") t1 t2
   (when angle1 (anim-linear-rotate-updater obj t1 t2 angle1 angle2))
   nil
   (anim-linear-rotate-finisher obj angle2)
   ))


(defn instant-rotate [obj t1 t2 angle]
  (timbre/trace "instant-rotate" obj t1 t2 angle)
  (add-animation!
   (object-animation-id obj "rot") t1 t2
   nil
   nil
   (anim-linear-rotate-finisher obj angle)
   ))


(defn linear-animate [aid t1 t2 v1 v2 setter]
  (let [ai (mk-linear-interpolator t1 t2 v1 v2)]
    (add-animation!
     aid t1 t2
     (fn [t] (setter (ai t)))
     nil
     (fn [] (setter v2)))))
