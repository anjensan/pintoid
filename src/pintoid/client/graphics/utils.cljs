(ns pintoid.client.graphics.utils
  (:require
   [cljsjs.pixi]
   [clojure.string :refer [upper-case]]))

(def ^:const pi js/Math.PI)
(def ^:const pi2 (* 2 pi))
(def ^:const +inf js/Infinity)
(def ^:const -inf (- js/Infinity))


(defn point->vec [pp]
  [(.-x pp) (.-y pp)])


(defn vec->point [[x y]]
  (js/PIXI.Point. x y))


(defn ->rectangle [v]
  (when v
    (if (map? v)
      (let [{:keys [x y h w]} v]
        (js/PIXI.Rectangle. x y w h))
      (let [[[x1 y1] [x2 y2]] v]
        (js/PIXI.Rectangle. x1 y1 (- x2 x1) (- y2 y1))))))


(defn minmax [x y]
  [(min x y) (max x y)])


(defn ->pair [xy]
  (when xy
    (if (sequential? xy)
      (let [[x y] xy] [x y])
      [xy xy])))


(defn ->point [xy]
  (cond
    (nil? xy) nil
    (sequential? xy) (let [[x y] xy] (js/PIXI.Point. x y))
    :else (let [a (float xy)] (js/PIXI.Point. a a))))


(defn ->blendmode [bm]
  (cond
    (nil? bm) js/PIXI.BLEND_MODES.NORMAL
    (keyword? bm) (aget js/PIXI.BLEND_MODES (upper-case (name bm)))
    (number? bm) bm
    :else (throw (ex-info "Invalid blendmode" {:bm bm}))))
