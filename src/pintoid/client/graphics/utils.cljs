(ns pintoid.client.graphics.utils)

(def ^:const pi js/Math.PI)
(def ^:const pi2 (* 2 pi))


(defn to-pair [xy]
  (if (sequential? xy)
    (let [[x y] xy] [x y])
    [xy xy]))


(defn to-point [xy]
  (if (sequential? xy)
    (let [[x y] xy] (js/PIXI.Point. x y))
    (let [a (float xy)] (js/PIXI.Point. a a))))
