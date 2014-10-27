(ns pintoid.server.ecs-test
  (:use clojure.test)
  (:use pintoid.server.ecs))

(deftest test-empty-ecs
  (let [ecs (create-ecs)]
    (is (nil? (seq (entity-ids ecs :x))))
    (is (nil? (seq (component-ids ecs 1))))
    (is (== 0 (count ecs)))
    (is (empty? ecs))))

(deftest test-create-ecs
  (let [ecs (create-ecs
             [[1 :x :x1]
              [2 :x :x2]
              [1 :y :y1]
              [3 :y :y3]])]
    (is (== 4 (count ecs)))
    (is (= :x1 (component ecs 1 :x)))
    (is (= :y1 (component ecs 1 :y)))
    (is (= :x2 (component ecs 2 :x)))
    (is (= :y3 (component ecs 3 :y)))
    (is (= #{1 2} (entity-ids ecs :x)))
    (is (= #{1 3} (entity-ids ecs :y)))
    (is (= #{:x :y} (component-ids ecs 1)))
    (is (= #{:x} (component-ids ecs 2)))
    (is (= #{:y} (component-ids ecs 3)))
    ))

(deftest test-put-comp-without-entity
  (let [ecs (-> (create-ecs) (put-component 1 1 1))]
    (is (nil? (component ecs 1 1)))
    (is (nil? (seq (component-ids ecs 1))))
    (is (nil? (seq (entity-ids ecs 1))))
    (is (empty? ecs))))

(deftest test-add-entity
  (let [ecs (-> (create-ecs)
                (add-entity 1 {:x :x1 :y :y1})
                (add-entity 3)
                (put-component 3 :y :y3)
                (add-entity 2 {:x :x2 :y :y2})
                (put-component 3 :x :x3)
                )]
    (is (== 6 (count ecs)))
    (is (= #{1 2 3} (entity-ids ecs :x) (entity-ids ecs :y)))
    (is (= #{:x :y} (component-ids ecs 1) (component-ids ecs 1)))))

(deftest test-drop-entity
  (let [ecs1 (-> (create-ecs)
                (add-entity 1)
                (put-component 1 :x :x1))
        ecs2 (-> ecs1
                 (drop-entity 1)
                 (put-component 1 :y :y2))]
    (is (= :x1 (component ecs1 1 :x)))
    (is (nil? (component ecs1 1 :y)))
    (is (nil? (component ecs2 1 :x)))
    (is (nil? (component ecs2 1 :y)))
    (is (== 1 (count ecs1)))
    (is (== 0 (count ecs2)))))



    
