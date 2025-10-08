(ns filtering.tests
  (:require [clojure.test :refer [deftest is run-tests]]
            [filtering.eager :as eager]
            [filtering.lazy :as lazy]))

(deftest test-with-coll
  (doseq [[pred coll] [[even? (list)]
                       [odd? (list)]
                       [even? (list 1)]
                       [even? (list 1 2)]
                       [even? (list 1 2 3)]
                       [even? (list 1 2 3 4)]
                       [even? (list 1 2 3 4 5)]
                       [even? (list 1 2 3 4 5 6)]
                       [even? (list 1 2 3 4 5 6 7)]
                       [even? (list 1 2 3 4 5 6 7 8)]
                       [even? (list 1 2 3 4 5 6 7 8 9)]
                       [even? (list 1 2 3 4 5 6 7 8 10)]
                       [(fn [_] true) (list 1 2 3 4 5 6 7 8 9 10)]]]
    (let [lib-result (filter pred coll)
          eager-result (eager/parallel-filter pred coll)]
      ;(is (= lib-result eager-result))
      (is (= lib-result eager-result)))))

(run-tests 'filtering.tests)
