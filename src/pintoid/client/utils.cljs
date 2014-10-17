(ns pintoid.client.utils)

(def obj-uid-counter (atom 0))

(defn obj-uid [obj]
  (let [aid (aget obj "__obj_uid")]
    (if aid
      aid
      (let [aid (str (swap! obj-uid-counter inc))]
        (aset obj "__obj_uid" aid)
        aid))))
