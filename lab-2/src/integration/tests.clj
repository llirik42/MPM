(ns integration.tests
  (:require [clojure.test :refer [deftest is run-tests]]
            [integration.utils :as utils]
            [integration.default :as default]
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

(let [i (default/antiderivative utils/complex 1)]
  (println "_______________NON_OPTIMIZED_______________")
  (time (i 150))
  (time (i 151))
  (time (i 152))
  (time (i 153))
  (time (i 154))
  (time (i 155))
  (time (i 156))
  (time (i 157))
  (time (i 158))
  (time (i 159))
  (time (i 160))
  (time (i 161))
  (time (i 162))
  (time (i 163))
  (time (i 164))
  (time (i 165))
  (time (i 165))
  (time (i 165))
  (time (i 165))
  (time (i 165))
  (println))

(let [i (memo/antiderivative utils/complex 1)]
  (println "_______________MEMOIZED_1_______________")
  (time (i 150))
  (time (i 151))
  (time (i 152))
  (time (i 153))
  (time (i 154))
  (time (i 155))
  (time (i 156))
  (time (i 157))
  (time (i 158))
  (time (i 159))
  (time (i 160))
  (time (i 161))
  (time (i 162))
  (time (i 163))
  (time (i 164))
  (time (i 165))
  (println))

(let [i (memo/antiderivative utils/complex 1)]
  (println "_______________MEMOIZED_2_______________")
  (time (i 150))
  (time (i 151))
  (time (i 152))
  (time (i 153))
  (time (i 154))
  (time (i 155))
  (time (i 156))
  (time (i 157))
  (time (i 158))
  (time (i 159))
  (time (i 160))
  (time (i 161))
  (time (i 162))
  (time (i 163))
  (time (i 164))
  (time (i 165))
  (println))

(let [i (lazy/antiderivative utils/complex 1)]
  (println "_______________LAZY_SEQ_1_______________")
  (time (i 150))
  (time (i 151))
  (time (i 152))
  (time (i 153))
  (time (i 154))
  (time (i 155))
  (time (i 156))
  (time (i 157))
  (time (i 158))
  (time (i 159))
  (time (i 160))
  (time (i 161))
  (time (i 162))
  (time (i 163))
  (time (i 164))
  (time (i 165))
  (println))

(let [i (lazy/antiderivative utils/complex 1)]
  (println "_______________LAZY_SEQ_2_______________")
  (time (i 150))
  (time (i 151))
  (time (i 152))
  (time (i 153))
  (time (i 154))
  (time (i 155))
  (time (i 156))
  (time (i 157))
  (time (i 158))
  (time (i 159))
  (time (i 160))
  (time (i 161))
  (time (i 162))
  (time (i 163))
  (time (i 164))
  (time (i 165))
  (println))

(run-tests 'integration.tests)
