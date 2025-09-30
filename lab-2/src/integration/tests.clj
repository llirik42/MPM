(ns integration.tests
  (:require [clojure.test :refer [deftest is run-tests]]
            [integration.utils :as utils]
            [integration.memo :as memo]))

(deftest test-const
  (let [ad (memo/antiderivative utils/const 1)]
    (doseq [[x expected] [[0 0]
                          [1 1]
                          [2 2]
                          [10 10]
                          [50 50]
                          [-1 -1]
                          [-2 -2]
                          [-50 -50]]]
      (is (= expected (ad x))))))

(run-tests 'integration.tests)
