(ns pintoid.server.ecs.dump
  (:use [pintoid.server.ecs core util]
        [clojure.algo.monads])
  (:require [clojure.data.int-map :as im])
  (:import [clojure.data.int_map PersistentIntMap]))

(with-monad (maybe-t state-m ::empty-dump)

  (defn d-map [f dm]
    (domonad [d dm] (eduction (map (fn [[k v]] [k (f v)])) d)))

  (defn d-filter [p dm]
    (domonad [d dm] (eduction (filter p) d)))

  (defn d-diff [dm]
    (domonad [new ((m-lift 1 to-int-map) dm)
              old (set-val ::d-diff-old new)]
      (edcat
        (eduction (comp (remove #(contains? new %)) (map #(vector % nil))) (keys old))
        (eduction (remove (fn [[k v]] (= v (get old k)))) new))))

  (defn d-when-not-identical [dm]
    (domonad [new dm, old (set-val ::d-changed-old new)]
      (if (identical? old new) ::empty-dump new)))
  )

(with-monad state-m

  (defn dumps-map [& [k m & r :as rr]]
    (if-not (seq rr)
      (m-result {})
      (domonad [v (with-state-field k m)
                z (apply dumps-map r)]
        (if-not (= v ::empty-dump)
          (assoc z k v)
          z))))

  (defn dump
    [w c & {filter :filter
            map :map
            diff :diff
            :or {diff true, icheck true}}]
    (cond->> (m-result (get-comp-map w c))
      (not filter) (d-when-not-identical)
      filter       (d-filter filter)
      diff         (d-diff)
      map          (d-map map)))
  )
