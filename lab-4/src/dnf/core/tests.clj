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

(deftest test-lneg
  (let [c (const 1) 
        l1 (lneg c)
        l2 (lneg l1)]
    (is (lneg? l1))
    (is (lneg? l2))
    (is (= (list c) (args l1)))
    (is (= (list l1) (args l2)))
    (doseq [p [const? variable? land? lor? limpl?]]
      (is (not (p l1)))
      (is (not (p l2))))))

(deftest test-land
  (let [c (const 1)
        v (variable ::A)
        l1 (land c v)
        l2 (land v c)
        l3 (land c v l1 l2)]
    (is (land? l1))
    (is (land? l2))
    (is (land? l3)) 
    (is (= (list c v) (args l1)))
    (is (= (list v c) (args l2))) 
    (is (= (list c v l1 l2) (args l3)))
    (doseq [p [const? variable? lneg? lor? limpl?]]
      (is (not (p l1)))
      (is (not (p l2)))
      (is (not (p l3))))))

(deftest test-lor
  (let [c (const 0)
        v (variable ::B)
        l1 (lor c v)
        l2 (lor v c)
        l3 (lor l1 l2 v c)]
    (is (lor? l1))
    (is (lor? l2))
    (is (lor? l3))
    (is (= (list c v) (args l1)))
    (is (= (list v c) (args l2)))
    (is (= (list l1 l2 v c) (args l3)))
    (doseq [p [const? variable? lneg? land? limpl?]]
      (is (not (p l1)))
      (is (not (p l2)))
      (is (not (p l3))))))

(deftest test-limpl
  (let [c (const 0)
        v (variable ::B)
        l1 (limpl c v)
        l2 (limpl v c)
        l3 (limpl l1 l2)]
    (is (limpl? l1))
    (is (limpl? l2))
    (is (limpl? l3))
    (is (= (list c v) (args l1)))
    (is (= (list v c) (args l2)))
    (is (= (list l1 l2) (args l3)))
    (doseq [p [const? variable? lneg? land? lor?]]
      (is (not (p l1)))
      (is (not (p l2)))
      (is (not (p l3))))))

(run-tests 'dnf.core.tests)
