(ns pintoid.server.ecs-test
  (:use clojure.test)
  (:use [pintoid.server.ecs core data]))

(deftest test-empty-ecs
  (let [ecs (create-ecs)]
    (is (nil? (seq (get-comp-map ecs :x))))
    (is (nil? (seq (get-entity-comps ecs 1))))
    (is (== 0 (count ecs)))
    (is (empty? ecs))))

(deftest test-create-ecs
  (let [ecs (create-ecs
             [[1 :x :x1]
              [2 :x :x2]
              [1 :y :y1]
              [3 :y :y3]])]
    (is (== 4 (count ecs)))
    (is (= :x1 (get-comp ecs 1 :x)))
    (is (= :y1 (get-comp ecs 1 :y)))
    (is (= :x2 (get-comp ecs 2 :x)))
    (is (= :y3 (get-comp ecs 3 :y)))
    (is (= #{:x :y} (get-entity-comps ecs 1)))
    (is (= #{:x} (get-entity-comps ecs 2)))
    (is (= #{:y} (get-entity-comps ecs 3)))
    (is (= {1 :x1 2 :x2} (get-comp-map ecs :x)))
    (is (= {1 :y1 3 :y3} (get-comp-map ecs :y)))
    ))

(deftest test-next-entity-id
  (dotimes [_ 100]
    (is (number? (next-entity)))
    (is (not= (next-entity) (next-entity)))
    (is (< (next-entity) (next-entity)))))

(deftest test-put-comp-without-entity
  (let [ecs (-> (create-ecs) (put-comp 1 1 1))]
    (is (nil? (get-comp ecs 1 1)))
    (is (nil? (seq (get-entity-comps ecs 1))))
    (is (nil? (seq (get-comp-map ecs 1))))
    (is (empty? ecs))))

(deftest test-add-entity
  (let [ecs (-> (create-ecs)
                (add-entity 1 {:x :x1 :y :y1})
                (add-entity 3)
                (put-comp 3 :y :y3)
                (add-entity 2 {:x :x2 :y :y2})
                (put-comp 3 :x :x3)
                )]
    (is (== 6 (count ecs)))
    (is (= #{1 2 3} (set (keys (get-comp-map ecs :x))) (set (keys (get-comp-map ecs :y)))))
    (is (= #{:x :y} (get-entity-comps ecs 1) (get-entity-comps ecs 1)))))

(deftest test-remove-entity
  (let [ecs1 (-> (create-ecs)
                (add-entity 1)
                (put-comp 1 :x :x1))
        ecs2 (-> ecs1
                 (remove-entity 1)
                 (put-comp 1 :y :y2))]
    (is (= :x1 (get-comp ecs1 1 :x)))
    (is (nil? (get-comp ecs1 1 :y)))
    (is (nil? (get-comp ecs2 1 :x)))
    (is (nil? (get-comp ecs2 1 :y)))
    (is (== 1 (count ecs1)))
    (is (== 0 (count ecs2)))))

(deftest test-transient-ecs
  (let [ecs (reduce add-entity (create-ecs) [1 2 3])]

    (testing "noop on transient ecs"
      (is (nil? (seq (-> ecs (transient) (persistent!))))))

    (testing "transient ecs is readable"
      (let [ecs2 (-> ecs
                     (transient)
                     (put-comp! 1 :x :x1)
                     (put-comp! 1 :y :y1)
                     (put-comp! 2 :x :x2)
                     (put-comp! 2 :y :y2))]
        (is (= #{:x :y} (get-entity-comps ecs2 1)))
        (is (= {1 :x1 2 :x2} (get-comp-map ecs2 :x)))
        (is (nil? (ecs2 1 :z)))
        (is (= :none (ecs2 1 :z :none)))
        (is (= :x1 (ecs2 1 :x)))
        (is (= :x2 (ecs2 2 :x)))))

    (testing "add components to transient ecs"
      (let [ecs2 (-> ecs
                     (transient)
                     (put-comp! 2 :x :x2b)
                     (put-comp! 1 :x :x1b)
                     (put-comp! 1 :x :x1)
                     (put-comp! 1 :y :y1)
                     (put-comp! 2 :x :x2)
                     (put-comp! 2 :y :y2)
                     (persistent!))]
        (is (== 4 (count ecs2)))
        (is (= #{:x :y} (get-entity-comps ecs2 1)))
        (is (= {1 :x1 2 :x2} (get-comp-map ecs2 :x)))))

    ))


(deftest test-ecs-like-fn
  (let [ecs (-> (create-ecs)
                (add-entity 1 {:x :x1 :y false}))]
    (testing "use ecs as 3-arity function"
      (is (= :x1 (ecs 1 :x)))
      (is (false? (ecs 1 :y)))
      (is (nil? (ecs 2 :x)))
      (is (nil? (ecs 1 :z))))

    (testing "use ecs as 4-arity function"
      (is (= :x1 (ecs 1 :x :none)))
      (is (false? (ecs 1 :y :none)))
      (is (= :none (ecs 2 :x :none)))
      (is (= :none (ecs 1 :z :none))))))

(deftest test-ecs-conj
  (let [ecs (reduce add-entity (create-ecs) [1 2 3])]

    (testing "conj triples to ces"
      (let [c1 (conj ecs [1 :x :x1b] [1 :x :x1] [2 :x :x2] [4 :x :x4])]
        (is (== 2 (count c1)))
        (is (= {1 :x1 2 :x2} (get-comp-map c1 :x)))
        (is (= :x1 (c1 1 :x)))
        (is (= :x2 (c1 2 :x)))))

    (testing "conj triples to transient ces"
      (let [c1 (persistent!
                (reduce
                 conj!
                 (transient ecs)
                 [[1 :x :x1b] [1 :x :x1] [2 :x :x2] [4 :x :x4]]))]
        (is (== 2 (count c1)))
        (is (= {1 :x1 2 :x2} (get-comp-map c1 :x)))
        (is (= :x1 (c1 1 :x)))
        (is (= :x2 (c1 2 :x)))))
    ))
