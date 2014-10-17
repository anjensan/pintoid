(ns pintoid.client.utils
  (:require [dommy.core :as d])
  (:require-macros
   [dommy.core :refer [sel1]]))


(defn panic! [msg]
  ;; TODO: stop drawing-loop
  (d/replace-contents!
   (sel1 :#content)
   [:div
    "Something goes wrong! Sorry :'("
    (pr-str msg)]))


(def obj-uid
  (let [cnt (atom 0)]
    (fn [obj]
      (let [aid (aget obj "__obj_uid")]
        (if aid
          aid
          (let [aid (str (swap! cnt inc))]
            (aset obj "__obj_uid" aid)
            aid))))))
