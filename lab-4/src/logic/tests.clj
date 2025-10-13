(ns logic.tests
  (:require [clojure.test :refer [deftest is run-tests]]
            [logic.core :refer :all]
            [logic.repr :refer [repr]]
            [logic.value :refer [value]]
            [logic.dnf :refer [dnf]]))

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
    (doseq [pred [variable? lneg? land? lor? limpl?]]
      (is (not (pred c1)))
      (is (not (pred c2)))
      (is (not (pred c3)))
      (is (not (pred c4))))))

(deftest test-variables
  (let [v1 (variable ::A)
        v2 (variable ::B)]
    (is (variable? v1))
    (is (variable? v2))
    (is (= ::A (variable-name v1)))
    (is (= ::B (variable-name v2)))
    (doseq [pred [const? lneg? land? lor? limpl?]]
      (is (not (pred v1)))
      (is (not (pred v2))))))

(deftest test-lneg
  (let [c (const 1)
        l1 (lneg c)
        l2 (lneg l1)]
    (is (lneg? l1))
    (is (lneg? l2))
    (is (= (list c) (args l1)))
    (is (= (list l1) (args l2)))
    (doseq [pred [const? variable? land? lor? limpl?]]
      (is (not (pred l1)))
      (is (not (pred l2))))))

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
    (doseq [pred [const? variable? lneg? lor? limpl?]]
      (is (not (pred l1)))
      (is (not (pred l2)))
      (is (not (pred l3))))))

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
    (doseq [pred [const? variable? lneg? land? limpl?]]
      (is (not (pred l1)))
      (is (not (pred l2)))
      (is (not (pred l3))))))

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
    (doseq [pred [const? variable? lneg? land? lor?]]
      (is (not (pred l1)))
      (is (not (pred l2)))
      (is (not (pred l3))))))

(deftest test-repr
  (let [c0 (const 0)
        c1 (const 1)
        v1 (variable ::A)
        v2 (variable ::B)
        land1 (land c0 c1 v1)
        lor1 (lor v1 v2 c1)
        neg1 (lneg c0)
        neg2 (lneg v1)
        neg3 (lneg land1)
        neg4 (lneg lor1)
        impl1 (limpl c0 v1)
        impl2 (limpl land1 v1)
        impl3 (limpl c0 lor1)
        complex (land lor1 neg1 neg4 impl1 (lor neg2 neg3))]
    (doseq [[expr expr-repr] [[c0 "0"]
                              [c1 "1"]
                              [v1 "A"]
                              [v2 "B"]
                              [land1 "(0 & 1 & A)"]
                              [lor1 "(A v B v 1)"]
                              [neg1 "¬0"]
                              [neg2 "¬A"]
                              [neg3 "¬(0 & 1 & A)"]
                              [neg4 "¬(A v B v 1)"]
                              [impl1 "(0 → A)"]
                              [impl2 "((0 & 1 & A) → A)"]
                              [impl3 "(0 → (A v B v 1))"]
                              [complex "((A v B v 1) & ¬0 & ¬(A v B v 1) & (0 → A) & (¬A v ¬(0 & 1 & A)))"]
                              [(lor (lneg (variable ::A)) (lneg (variable ::B)) (lneg (variable ::C))) "(¬A v ¬B v ¬C)"]]]
      (is (= expr-repr (repr expr))))))

(deftest test-value
  (let [t (const 1)
        f (const 0)
        ctx {:a 1, :b 0, :c true, :d false}]
    (doseq [[expr expr-value] [[t true]
                               [f false]
                               [(variable :a) true]
                               [(variable :b) false]
                               [(variable :c) true]
                               [(variable :d) false]
                               [(lneg t) false]
                               [(lneg f) true]
                               [(land f f) false]
                               [(land f t) false]
                               [(land t f) false]
                               [(land t t) true]
                               [(lor f f) false]
                               [(lor f t) true]
                               [(lor t f) true]
                               [(lor t t) true]
                               [(limpl f f) true]
                               [(limpl f t) true]
                               [(limpl t f) false]
                               [(limpl t t) true]]]
      (is (= expr-value (value expr ctx))))))

(deftest test-dnf
  (let [a (variable ::A)
        b (variable ::B)
        c (variable ::C)
        d (variable ::D)
        e (variable ::E)
        t (const 1)
        f (const 0)]
  (doseq [[expr expr-dnf] [[(land a b) (land a b)] ; A & B ~ A & B
                           [(lor a b) (lor a b)] ; A v B ~ A v B
                           [(lneg a) (lneg a)] ; ¬A ~ ¬A 
                           [(limpl a b) (lor (lneg a) b)] ; A → B ~ ¬A v B
                           [(lneg (land a b)) (lor (lneg a) (lneg b))] ; ¬(A & B) ~ ¬A v ¬B
                           [(lneg (lor a b)) (land (lneg a) (lneg b))] ; ¬(A v B) ~ ¬A & ¬B
                           [(lor (land a b) c) (lor (land a b) c)] ; (A & B) v C ~ (A & B) v C
                           [(land a (lor b c)) (lor (land a b) (land a c))] ; A & (B v C) ~ (A & B) v (A & C)
                           [(land (lor a b) (lor c d)) (lor (land a c) (land b c) (land a d) (land b d))] ; (A v B) & (C v D) ~ (A & C) v (B & C) v (A & D) v (B & D)
                           [(land (limpl a b) c) (lor (land (lneg a) c) (land b c))] ; (A → B) ∧ C ~ (¬A & C) v (B & C)
                           [(limpl a (lor b c)) (lor (lneg a) b c)] ; A → (B ∨ C) ~ (¬A v B v C)
                           [(lneg (limpl a b)) (land a (lneg b))] ; ¬(A → B) ~ (A & ¬B)
                           [(limpl a (limpl b c)) (lor (lneg a) (lneg b) c)] ; (A → (B → C)) ~ (¬A v ¬B v C)
                           [(lneg (land a (lor b (lneg c)))) (lor (lneg a) (land (lneg b) c))] ; ¬(A ∧ (B ∨ ¬C)) ~ (¬A v (¬B & C))
                           ;[(land (lor a (land b c)) (lor (lneg a) d)) a] ; (A ∨ (B ∧ C)) ∧ (¬A ∨ D) ~ 



                           ]]
    (is (= expr-dnf (dnf expr)))
    ;(println (repr expr) "!" (repr expr-dnf) "!" (repr (dnf expr)))
    )))

(run-tests 'logic.tests)
