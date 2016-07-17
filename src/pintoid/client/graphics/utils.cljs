(ns pintoid.client.graphics.utils)

(def ^:const pi js/Math.PI)
(def ^:const pi2 (* 2 pi))
(def ^:const +inf js/Infinity)
(def ^:const -inf (- js/Infinity))


(defn point->vec [pp]
  [(.-x pp) (.-y pp)])


(defn vec->point [[x y]]
  (js/PIXI.Point. x y))


(defn minmax [x y]
  [(min x y) (max x y)])


(defn to-pair [xy]
  (if (sequential? xy)
    (let [[x y] xy] [x y])
    [xy xy]))


(defn to-point [xy]
  (if (sequential? xy)
    (let [[x y] xy] (js/PIXI.Point. x y))
    (let [a (float xy)] (js/PIXI.Point. a a))))
