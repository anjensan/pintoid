(ns pintoid.client.utils
  (:require [dommy.core :as d])
  (:require-macros
   [dommy.core :refer [sel1]]))


(defn map-val
  ([f] (map (fn [[k v]] [k (f v)])))
  ([f m] (into (empty m) (map-val f) m)))


(defn vcall
  ([f v] (map f v))
  ([f a1 v] (map #(f a1 %) v))
  ([f a1 a2 v] (map #(f a1 a2 %) v))
  ([f a1 a2 a3 v] (map #(f a1 a2 a3 %) v))
  ([f a1 a2 a3 a4 & asv]
   (let [as (vec (butlast asv)) v (last asv)]
     (map #(apply f a1 a2 a3 a4 (conj as %)) v))))


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
  ;; TODO: stop drawing-loop
  (d/replace-contents!
   (sel1 :#content)
   [:div
    "Something goes wrong! Sorry :'("
    (pr-str msg)]))


