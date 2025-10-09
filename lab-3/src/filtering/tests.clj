(ns filtering.tests
  (:require [clojure.test :refer [deftest is run-tests]]
            [filtering.impl :refer [pfilter]]))

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

(run-tests 'filtering.tests)
