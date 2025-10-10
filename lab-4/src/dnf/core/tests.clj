(ns dnf.core.tests
  (:require [clojure.test :refer [deftest is run-tests]]
            [dnf.core.core :refer :all]))

(deftest test-constants
  (let [c1 (const 0)
        c2 (const false)
        c3 (const 1)
        c4 (const true)]
    (is (const? c1))
    (is (const? c2))
    (is (const? c3))
    (is (const? c4))
    (is (= false (const-value c1)))
    (is (= false (const-value c2)))
    (is (= true (const-value c3)))
    (is (= true (const-value c4)))
    (doseq [p [variable? lneg? land? lor? limpl?]]
      (is (not (p c1)))
      (is (not (p c2)))
      (is (not (p c3)))
      (is (not (p c4))))))

(deftest test-variables
  (let [v1 (variable ::A)
        v2 (variable ::B)]
    (is (variable? v1))
    (is (variable? v2))
    (is (= ::A (variable-name v1)))
    (is (= ::B (variable-name v2)))
    (doseq [p [const? lneg? land? lor? limpl?]]
      (is (not (p v1)))
      (is (not (p v2))))))

(run-tests 'dnf.core.tests)
