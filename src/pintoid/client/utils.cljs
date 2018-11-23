(ns pintoid.client.utils
  (:require
   [cljsjs.pixi]
   [clojure.string :refer [upper-case]]
   [dommy.core :as d])
  (:require-macros
   [dommy.core :refer [sel1]]))

(def ^:const pi js/Math.PI)
(def ^:const pi2 (* 2 pi))
(def ^:const +inf js/Infinity)
(def ^:const -inf (- js/Infinity))

(defn map-val
  ([f] (map (fn [[k v]] [k (f v)])))
  ([f m] (into (empty m) (map-val f) m)))

(defn transpose-mom [mom]
  "Transpose map of maps.
   => (transpose-mom {:a {1 2 3 4}, :b {1 :x 2 :y}})
   {1 {:a 2 :b :x}, 2 {:b :y}, 3 {:a 4}}"
  (->>
   mom
   (reduce
    (fn [r [k1 m]]
      (reduce
       (fn [r' [k2 v]]
         (let [m (or (get r' k2) (transient {}))]
           (assoc! r' k2 (assoc! m k1 v))))
       r
       m))
    (transient {}))
   (persistent!)
   (into {} (map-val persistent!))))

(defn limit-str [n & ss]
  (let [s (apply str ss)]
    (if (> (count s) n)
      (str (subs s 0 n) "...")
      s)))

(defn panic! [msg]
  ;; TODO: Stop drawing-loop
  (d/replace-contents!
   (sel1 :#content)
   [:div
    "Something goes wrong! Sorry :'("
    (pr-str msg)]))

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
