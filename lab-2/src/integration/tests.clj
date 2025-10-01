(ns integration.tests
  (:require [clojure.test :refer [deftest is run-tests]]
            [integration.utils :as utils]
            [integration.memo :as memo]
            [integration.lazy :as lazy]))

(deftest test-const
  (let [f utils/const
        h 0.5
        ad-memo (memo/antiderivative f h)
        ad-lazy (lazy/antiderivative f h)]
    (doseq [[x expected] [[0 0]
                          [0.1 0]
                          [0.4 0.5]
                          [0.5 0.5]
                          [0.6 0.5]
                          [0.9 1]
                          [1 1]
                          [2 2]
                          [10 10]
                          [50 50]
                          [-0.1 0]
                          [-0.4 -0.5]
                          [-0.5 -0.5]
                          [-0.6 -0.5]
                          [-0.9 -1]
                          [-1 -1]
                          [-2 -2]
                          [-50 -50]]]
      (is (= (double expected) (double (ad-memo x))))
      (is (= (double expected) (double (ad-lazy x)))))))

(deftest test-linear
  (let [f utils/linear
        h 0.5
        ad-memo (memo/antiderivative f h)
        ad-lazy (lazy/antiderivative f h)]
    (doseq [[x expected] [[0 0]
                          [0.1 0]
                          [0.4 0.25]
                          [0.5 0.25]
                          [0.6 0.25]
                          [0.9 1]
                          [1 1]
                          [2 4]
                          [3 9]
                          [4 16]
                          [10 100]
                          [25 625]
                          [37.25 1406.25]
                          [37.5 1406.25]
                          [37.75 1444]
                          [50 2500]
                          [-0.1 0]
                          [-0.4 0.25]
                          [-0.5 0.25]
                          [-0.6 0.25]
                          [-0.9 1]
                          [-1 1]
                          [-2 4]
                          [-3 9]
                          [-4 16]
                          [-13.25 169]
                          [-13.5 182.25]
                          [-13.75 182.25]
                          [-25 625]
                          [-50 2500]]]
      (is (= (double expected) (double (ad-memo x))))
      (is (= (double expected) (double (ad-lazy x)))))))

(run-tests 'integration.tests)
