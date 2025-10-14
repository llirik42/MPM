(ns logic.tests
  (:require [clojure.test :refer [deftest is run-tests]]
            [logic.core :refer :all]
            [logic.repr :refer [repr]]
            [logic.value :refer [value]]
            [logic.dnf :refer [dnf]]))

(deftest test-constant-creation
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
    (doseq [false-pred [variable? lneg? land? lor? limpl?]]
      (is (not (false-pred c1)))
      (is (not (false-pred c2)))
      (is (not (false-pred c3)))
      (is (not (false-pred c4))))))

(deftest test-variable-creation
  (let [v1 (variable ::A)
        v2 (variable ::B)]
    (is (variable? v1))
    (is (variable? v2))
    (is (= ::A (variable-name v1)))
    (is (= ::B (variable-name v2)))
    (doseq [false-pred [const? lneg? land? lor? limpl?]]
      (is (not (false-pred v1)))
      (is (not (false-pred v2))))))

(deftest test-negation-creation
  (let [c (const 1)
        l1 (lneg c)
        l2 (lneg l1)]
    (is (lneg? l1))
    (is (lneg? l2))
    (is (= (list c) (args l1)))
    (is (= (list l1) (args l2)))
    (doseq [false-pred [const? variable? land? lor? limpl?]]
      (is (not (false-pred l1)))
      (is (not (false-pred l2))))))

(deftest test-conjuction-creations
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
    (doseq [false-pred [const? variable? lneg? lor? limpl?]]
      (is (not (false-pred l1)))
      (is (not (false-pred l2)))
      (is (not (false-pred l3))))))

(deftest test-disjunction-creation
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
    (doseq [false-pred [const? variable? lneg? land? limpl?]]
      (is (not (false-pred l1)))
      (is (not (false-pred l2)))
      (is (not (false-pred l3))))))

(deftest test-implication-creation
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
    (doseq [false-pred [const? variable? lneg? land? lor?]]
      (is (not (false-pred l1)))
      (is (not (false-pred l2)))
      (is (not (false-pred l3))))))

(deftest test-repr
  (let [f (const 0)
        t (const 1)
        a (variable ::A)
        b (variable ::B)]
    (doseq [[expr expected-repr] [[f "0"]
                                  [t "1"]
                                  [a "A"]
                                  [b "B"]
                                  [(land f t a) "(0 & 1 & A)"]
                                  [(lor a b t) "(A v B v 1)"]
                                  [(lneg f) "¬0"]
                                  [(lneg a) "¬A"]
                                  [(lneg (land f t a)) "¬(0 & 1 & A)"]
                                  [(lneg (lor a b t)) "¬(A v B v 1)"]
                                  [(limpl f a) "(0 → A)"]
                                  [(limpl (land f a b) a) "((0 & A & B) → A)"]
                                  [(limpl f (lor a b t)) "(0 → (A v B v 1))"]
                                  [(land (lor a b t) (lneg f) (lneg (lor a b t)) (limpl f a) (lor (lneg a) (lneg (land f t a)))) "((A v B v 1) & ¬0 & ¬(A v B v 1) & (0 → A) & (¬A v ¬(0 & 1 & A)))"]
                                  [(lor (lneg (variable ::A)) (lneg (variable ::B)) (lneg (variable ::C))) "(¬A v ¬B v ¬C)"]]]
      (is (= expected-repr (repr expr))))))

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
                             [(land (lor a (land b c)) (lor (lneg a) d)) (lor (land b c (lneg a)) (land a d) (land b c d))] ; (A ∨ (B ∧ C)) ∧ (¬A ∨ D) ~ 

                             [(land (limpl a b) (limpl c d)) (lor (land (lneg a) (lneg c)) (land b (lneg c)) (land (lneg a) d) (land b d))] ; (A → B) ∧ (C → D) ~ (¬A & ¬C) v (¬A & D) v (B & ¬C) v (B & D)

                             ;[(land (lor a (lneg b)) (lor b (lneg c)) (lor c (lneg a))) (lor (land a b c) (land (lneg a) (lneg b) (lneg c)))] ;TODO: fix. Must be: (A & B & ~A) ~ 0

                             [(lneg (lor (land a b) (land (lneg c) d))) (lor (land (lneg a) c) (land (lneg b) c) (land (lneg a) (lneg d)) (land (lneg b) (lneg d)))] ;2

                             [(lor (limpl a (land b (lneg c))) (land (lneg a) (limpl c d))) (lor (lneg a) (land b (lneg c)) (land (lneg a) (lneg c)) (land (lneg a) d))] ;3

                             [(lneg (lor (limpl a b) (limpl c (lneg d)))) (land a (lneg b) c d)] ;4

                             [(limpl (land (lor a (lneg b)) (lor (lneg a) c)) (lor d (lneg e))) (lor (land (lneg a) b) (land a (lneg c)) d (lneg e))] ;5

                             [(land (lor a b c) (lor (lneg a) (lneg b) d)) (lor (land b (lneg a)) (land c (lneg a)) (land a (lneg b)) (land c (lneg b)) (land a d) (land b d) (land c d))] ;6

                             [(lneg (land a (limpl b (lor c (lneg d))))) (lor (lneg a) (land b (lneg c) d))] ;7 

                             [(land a t) a] ;8

                             [(land a f) f] ;9

                             [(lor a f) a] ;10

                             [(lor a t) t] ;11

                             [(limpl f t) t] ;12

                             [(limpl f f) t] ;13

                             [(limpl t f) f] ;14

                             [(limpl t t) t] ;15

                             [(lneg t) f] ;16

                             [(lneg f) t] ;17

                             [(land (lor a b) t) (lor a b)] ;18

                             [(lor (land a b) f) (land a b)] ;19

                             [(land (lor a f) (lor b t)) (lor (land a b) a)] ;20. TODO: реализовать закон поглощения

                             [(lor (land a t) (land b f)) a] ;21

                             [(lor (land (lneg a) f) b) b] ;22

                             [(lneg (lor a f)) (lneg a)] ;23

                             [(lneg (land a t)) (lneg a)] ;24

                             [(land (lor a t) (lor b f)) (lor (land a b) b)] ;25. TODO: реализовать закон поглощения 

                             [(lor (land a (lor b f)) (land c t)) (lor (land a b) c)] ;26

                             [(lor (land a (lneg b)) (land f c)) (land a (lneg b))] ;27

                             [(lor (lor a (land b f)) (land c t)) (lor a c)] ;28

                             [(land (limpl a b) t) (lor (lneg a) b)] ;29

                             [(limpl a (lor b f)) (lor (lneg a) b)] ;30

                             [(lneg (lor (land a f) (land b t))) (lor (land (lneg a) (lneg b)) (lneg b))] ;31

                             [(lor (limpl t a) (limpl f b)) t] ;32

                             [(land (limpl a t) (limpl b f)) (lor (land (lneg a) (lneg b)) (lneg b))] ;33

                             [(lor (land a (limpl b f)) (land (lneg c) t)) (lor (land a (lneg b)) (lneg c))] ;35

                             [(land (lor a f) (lor b (land c t))) (lor (land a b) (land a c))] ;36

                             [(limpl (lor (land a f) (land b t)) (lor t c)) t] ;37

                             [(lneg (lor (land a t) (land f (lneg b)))) (lor (lneg a) (land (lneg a) b))] ;38. TODO: при добавлении закона поглощения нужно изменить этот тест

                             [(lor (land (lor a b) (lor f c)) (land (lneg a) t)) (lor (land a c) (land b c) (lneg a))] ;39

                             [(land (limpl (lor b f) a) (limpl c t)) (lor (land (lneg b) (lneg c)) (land a (lneg c)) (lneg b) a)] ;40. TODO: закон поглощения

                             [(land (lor a (land b (lor c f))) (limpl d t)) (lor (land a (lneg d)) a (land b c (lneg d)) (land b c))] ;41

                             [(lor a a) (lor a a)] ;42. TODO: A v A ~ A v A (нужно фиксить)

                             [(land a a) (land a a)] ;43. TODO: A & A ~ A & A (нужно фиксить)

                             [(lor (land a b) (land a b)) (lor (land a b) (land a b))] ;44. TODO: (A & B) v (A & B) ~ (A & B) v (A & B) (нужно фиксить)

                             [(land a (lneg a)) f] ;45

                             [(lor (land a (lneg a)) b) b] ;46

                             [(lor (land a (lneg a)) (land b c)) (land b c)] ;47

                             [(lor (land a b) a) (lor (land a b) a)] ;48. TODO: закон поглощения

                             [(lor (land a b) (land a b c)) (lor (land a b) (land a b c))] ;49. TODO: закон поглощения

                             [(lor a (land a b)) (lor a (land a b))] ;50. TODO: закон поглощения

                             [(lor a (lneg a)) (lor a (lneg a))] ;51. TODO: добавить обработку тавтологий (A v ¬A ~ 1)

                             [(land a f) f] ;52

                             ;[(lor (land a b) (land a b)) f]

                            ; [(land (limpl a b) (limpl (lneg b) (lneg a))) (limpl a b)]

                             ;[(lor (limpl a b) (limpl (lneg b) (lneg a))) (limpl a b)]
                             ]]
      (is (= expr-dnf (dnf expr))) ; TODO: при сравнении логических выражений использовать специальную функцию, поскольку например (A & B) и (B & A) это суть одно и то же, однако при сравнении с помощью "=" будет false
      )))

(run-tests 'logic.tests)
