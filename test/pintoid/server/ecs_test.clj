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

(deftest test-next-entity-id
  (dotimes [_ 100]
    (is (number? (next-entity-id)))
    (is (not= (next-entity-id) (next-entity-id)))
    (is (< (next-entity-id) (next-entity-id)))))

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

(deftest test-transient-ecs
  (let [ecs (reduce add-entity (create-ecs) [1 2 3])]

    (testing "noop on transient ecs"
      (is (nil? (seq (-> ecs (transient) (persistent!))))))

    (testing "transient ecs is readable"
      (let [ecs2 (-> ecs
                     (transient)
                     (put-component! 1 :x :x1)
                     (put-component! 1 :y :y1)
                     (put-component! 2 :x :x2)
                     (put-component! 2 :y :y2))]
        (is (= #{:x :y} (component-ids ecs2 1)))
        (is (= #{1 2} (entity-ids ecs2 :x)))
        (is (nil? (ecs2 1 :z)))
        (is (= :none (ecs2 1 :z :none)))
        (is (= :x1 (ecs2 1 :x)))
        (is (= :x2 (ecs2 2 :x)))))

    (testing "add components to transient ecs"
      (let [ecs2 (-> ecs
                     (transient)
                     (put-component! 2 :x :x2b)
                     (put-component! 1 :x :x1b)
                     (put-component! 1 :x :x1)
                     (put-component! 1 :y :y1)
                     (put-component! 2 :x :x2)
                     (put-component! 2 :y :y2)
                     (persistent!))]
        (is (== 4 (count ecs2)))
        (is (= #{:x :y} (component-ids ecs2 1)))
        (is (= #{1 2} (entity-ids ecs2 :x)))))

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
        (is (= #{1 2} (entity-ids c1 :x)))
        (is (= :x1 (c1 1 :x)))
        (is (= :x2 (c1 2 :x)))))

    (testing "conj triples to transient ces"
      (let [c1 (persistent!
                (reduce
                 conj!
                 (transient ecs)
                 [[1 :x :x1b] [1 :x :x1] [2 :x :x2] [4 :x :x4]]))]
        (is (== 2 (count c1)))
        (is (= #{1 2} (entity-ids c1 :x)))
        (is (= :x1 (c1 1 :x)))
        (is (= :x2 (c1 2 :x)))))
    ))

(deftest test-eids-operations

  (testing "create eid-sets"
    (is (empty? (eids)))
    (is (nil? (seq (eids))))
    (is (= #{1 2 3} (eids [1 3 2 1]))))

  (testing "eids is sorted"
    (let [rs (shuffle (range 100))
          ss (sort rs)]
      (is (= ss (seq (eids rs))))))

  (let [a (eids (range 10))
        b (eids (filter odd? (range 10)))
        c (eids (filter even? (range 10)))
        d (eids (range 5))]

    (testing "create eid-sets"
      (is (== 10 (count a)))
      (is (== 5 (count b) (count c) (count d)))
      (is (= a (into (eids) (range 10)))))

    (testing "eids union"
      (is (= #{1 2 3} (eids+ (eids [1]) (eids [2]) (eids [3]))))
      (is (= a (eids+ a) (eids+ b c) (eids+ b b c d))))

    (testing "eids intersection"
      (is (= #{} (eids* b c))) 
      (is (= #{0 2 4} (eids* c d))) 
      (is (= #{1 3} (eids* b d))))

    (testing "eids difference"
      (is (= #{5 6 7 8 9} (eids- a d)))
      (is (= #{5 7 9} (eids- a d c))))
      (is (= #{} (eids- a a)))
    ))
