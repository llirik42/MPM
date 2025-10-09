(ns filtering.tests
  (:require [clojure.test :refer [deftest is run-tests]]
            [filtering.impl :refer [pfilter]]
            [filtering.utils :refer [naturals heavy-pred]]))

(deftest test-range
  (doseq [[pred max-range] [[even? 1]
                            [odd? 1]
                            [even? 2]
                            [even? 2]
                            [even? 3]
                            [even? 3]
                            [even? 4]
                            [even? 4]
                            [even? 5]
                            [even? 6]
                            [even? 7]
                            [even? 8]
                            [even? 9]
                            [(fn [_] true) 1000]]]
    (let [coll (range max-range)
          lib-result (filter pred coll)
          impl-result (pfilter pred coll)]
      (is (= lib-result impl-result)))))

(deftest test-naturals
  (let [pred (fn [_] true)
        lib-result (filter pred naturals)
        impl-result (pfilter pred naturals)]
    (doseq [n [1 2 3 4 5 6 7 8 9 10 100 1000 10000 100000]]
      (let [lib-nth (nth lib-result n)
            impl-nth (nth impl-result n)]
        (is (= lib-nth impl-nth))))))

(run-tests 'filtering.tests)

(println "\n--------------TESTING-OF-TIME--------------")
(let [r (range 16)
      block-size 1]
  (time (doall (pfilter heavy-pred r block-size)))
  (time (doall (filter heavy-pred r))))
