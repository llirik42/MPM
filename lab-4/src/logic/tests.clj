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
                        ;;    [(land (lor a (land b c)) (lor (lneg a) d)) a] ; (A ∨ (B ∧ C)) ∧ (¬A ∨ D) ~ 

                        [(land (limpl a b) (limpl c d)) (lor (land (lneg a) (lneg c)) (land b (lneg c)) (land (lneg a) d) (land b d))] ; (A → B) ∧ (C → D) ~ (¬A & ¬C) v (¬A & D) v (B & ¬C) v (B & D)

                        ;;    [(land (lor a (lneg b)) (lor b (lneg c)) (lor c (lneg a))) a] ;1

                        ;; [(lneg (lor (land a b) (land (lneg c) d))) a] ;2

                        ;;    [(lor (limpl a (land b (lneg c))) (land (lneg a) (limpl c d))) a] ;3

                        ;;    [(lneg (lor (limpl a b) (limpl c (lneg d)))) a] ;4

                        ;;    [(limpl (land (lor a (lneg b)) (lor (lneg a) c)) (lor d (lneg e))) a] ;5

                        ;;    [(land (lor a b c) (lor (lneg a) (lneg b) d)) a] ;6

                        ;;    [(lneg (land a (limpl b (lor c (lneg d))))) a] ;7 

                        ;;    [(land a t) a] ;8

                        ;;    [(land a f) a] ;9

                        ;;    [(lor a f) a] ;10

                        ;;    [(lor a t) a] ;11

                        ;;    [(limpl f t) a] ;12

                        ;;    [(limpl f f) a] ;13

                        ;;    [(limpl t f) a] ;14

                        ;; [(limpl t t) t] ;15

                        ;; [(lneg t) f] ;16

                        ;; [(lneg f) t] ;17

                        ;; [(land (lor a b) t) (lor a b)] ;18

                        ;; [(lor (land a b) f) (land a b)] ;19

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
                        
                        ;[(land (lor a (land b (lor c f))) (limpl d t)) a] ;41

                        ;[(lor a a) a] ;42

                        ;;    [(land a a) a] ;43

                        ;;    [(lor (land a b) (land a b)) a] ;44

                        ;;    [(land a (lneg a)) a] ;45

                        ;;    [(lor (land a (lneg a)) b) a] ;46

                        ;;    [(lor (land a (lneg a)) (land b c)) a] ;47

                        ;;    [(lor (land a b) a) a] ;48

                        ;;    [(lor (land a b) (land a b c)) a] ;49

                        ;;    [(lor a (land a b)) a] ;50

                        ;;    [(lor a (lneg a)) a] ;51

                        ;[(land a f) f] ;52




                           ]]
    (is (= expr-dnf (dnf expr))) ; TODO: при сравнении логических выражений использовать специальную функцию, поскольку например (A & B) и (B & A) это суть одно и то же, однако при сравнении с помощью "=" будет false
    ;(println (repr expr) "!" (repr expr-dnf) "!" (repr (dnf expr)))
    )))

(run-tests 'logic.tests)
