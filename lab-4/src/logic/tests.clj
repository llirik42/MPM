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
    (doseq [[expr expected-value] [[t true]
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
      (is (= expected-value (value expr ctx))))))

(deftest test-dnf
  (let [a (variable ::A)
        b (variable ::B)
        c (variable ::C)
        d (variable ::D)
        e (variable ::E)
        neg-a (lneg a)
        neg-b (lneg b)
        neg-c (lneg c)
        neg-d (lneg d)
        neg-e (lneg e)
        t (const 1)
        f (const 0)]
    (doseq [[expr expected-dnf] [[(land a b)
                                  (land a b)]

                                 [(lor a b)
                                  (lor a b)]

                                 [neg-a
                                  neg-a]

                                 [(lneg neg-a)
                                  a]

                                 [(land (lneg neg-a) (lneg neg-b))
                                  (land a b)]

                                 [(limpl a b)
                                  (lor neg-a b)]

                                 [(lneg (land a b))
                                  (lor neg-a neg-b)]

                                 [(lneg (lor a b))
                                  (land neg-a neg-b)]

                                 [(lor (land a b) c)
                                  (lor (land a b) c)]

                                 [(land a (lor b c))
                                  (lor (land a b) (land a c))]

                                 [(land (lor a b) (lor c d))
                                  (lor (land a c) (land b c) (land a d) (land b d))]

                                 [(land (limpl a b) c)
                                  (lor (land neg-a c) (land b c))]

                                 [(limpl a (lor b c))
                                  (lor neg-a b c)]

                                 [(lneg (limpl a b))
                                  (land a neg-b)]

                                 [(limpl a (limpl b c))
                                  (lor neg-a neg-b c)]

                                 [(lneg (land a (lor b neg-c)))
                                  (lor neg-a (land neg-b c))]

                                 [(land (lor a (land b c)) (lor neg-a d))
                                  (lor (land b c neg-a) (land a d) (land b c d))]

                                 [(land (limpl a b) (limpl c d))
                                  (lor (land neg-a neg-c) (land b neg-c) (land neg-a d) (land b d))]

                                 [(lneg (lor (land a b) (land neg-c d)))
                                  (lor (land neg-a c) (land neg-b c) (land neg-a neg-d) (land neg-b neg-d))]

                                 [(lor (limpl a (land b neg-c)) (land neg-a (limpl c d)))
                                  (lor neg-a (land b neg-c) (land neg-a neg-c) (land neg-a d))]

                                 [(lneg (lor (limpl a b) (limpl c neg-d)))
                                  (land a neg-b c d)]

                                 [(limpl (land (lor a neg-b) (lor neg-a c)) (lor d neg-e))
                                  (lor (land neg-a b) (land a neg-c) d neg-e)]

                                 [(land (lor a b c) (lor neg-a neg-b d))
                                  (lor (land b neg-a) (land c neg-a) (land a neg-b) (land c neg-b) (land a d) (land b d) (land c d))]

                                 [(lneg (land a (limpl b (lor c neg-d))))
                                  (lor neg-a (land b neg-c d))]

                                 [(land a t)
                                  a]

                                 [(land a f)
                                  f]

                                 [(lor a f)
                                  a]

                                 [(lor a t)
                                  t]

                                 [(limpl f t)
                                  t]

                                 [(limpl f f)
                                  t]

                                 [(limpl t f)
                                  f]

                                 [(limpl t t)
                                  t]

                                 [(lneg t)
                                  f]

                                 [(lneg f)
                                  t]

                                 [(land (lor a b) t)
                                  (lor a b)]

                                 [(lor (land a b) f)
                                  (land a b)]

                                 ;; TODO: change this test after adding support of absorption law.
                                 [(land (lor a f) (lor b t))
                                  (lor (land a b) a)]

                                 [(lor (land a t) (land b f))
                                  a]

                                 [(lor (land neg-a f) b)
                                  b]

                                 [(lneg (lor a f))
                                  neg-a]

                                 [(lneg (land a t))
                                  neg-a]

                                 ;; TODO: change this test after adding support of absorption law.
                                 [(land (lor a t) (lor b f))
                                  (lor (land a b) b)] 

                                 [(lor (land a (lor b f)) (land c t))
                                  (lor (land a b) c)]

                                 [(lor (land a neg-b) (land f c))
                                  (land a neg-b)]

                                 [(lor (lor a (land b f)) (land c t))
                                  (lor a c)]

                                 [(land (limpl a b) t)
                                  (lor neg-a b)]

                                 [(limpl a (lor b f))
                                  (lor neg-a b)]

                                 [(lneg (lor (land a f) (land b t)))
                                  (lor (land neg-a neg-b) neg-b)]

                                 [(lor (limpl t a) (limpl f b))
                                  t]

                                 [(land (limpl a t) (limpl b f))
                                  (lor (land neg-a neg-b) neg-b)]

                                 [(lor (land a (limpl b f)) (land neg-c t))
                                  (lor (land a neg-b) neg-c)]

                                 [(land (lor a f) (lor b (land c t)))
                                  (lor (land a b) (land a c))]

                                 [(limpl (lor (land a f) (land b t)) (lor t c))
                                  t]

                                 ;; TODO: change this test after adding support of absorption law.
                                 [(lneg (lor (land a t) (land f neg-b)))
                                  (lor neg-a (land neg-a b))]

                                 [(lor (land (lor a b) (lor f c)) (land neg-a t))
                                  (lor (land a c) (land b c) neg-a)]

                                 ;; TODO: change this test after adding support of absorption law.
                                 [(land (limpl (lor b f) a) (limpl c t))
                                  (lor (land neg-b neg-c) (land a neg-c) neg-b a)]

                                 [(land (lor a (land b (lor c f))) (limpl d t))
                                  (lor (land a neg-d) a (land b c neg-d) (land b c))]

                                 ;; TODO: change this test after adding support of idempotence rules.
                                 [(lor a a)
                                  (lor a a)]

                                 ;; TODO: change this test after adding support of idempotence rules.
                                 [(land a a)
                                  (land a a)]

                                 ;; TODO: change this test after adding support of idempotence rules.
                                 [(lor (land a b) (land a b))
                                  (lor (land a b) (land a b))]

                                 [(land a neg-a)
                                  f]

                                 [(lor (land a neg-a) b)
                                  b]

                                 [(lor (land a neg-a) (land b c))
                                  (land b c)]

                                 ;; TODO: change this test after adding support of absorption law.
                                 [(lor (land a b) a)
                                  (lor (land a b) a)]

                                 ;; TODO: change this test after adding support of absorption law.
                                 [(lor (land a b) (land a b c))
                                  (lor (land a b) (land a b c))]

                                 ;; TODO: change this test after adding support of absorption law.
                                 [(lor a (land a b))
                                  (lor a (land a b))]

                                 ;; TODO: change this test after adding support of `A v ¬A` ~ `1`.
                                 [(lor a neg-a)
                                  (lor a neg-a)]

                                 [(land a f)
                                  f]]]
      
      ;; TODO: сompare expressions using a special function, rather than using =, since different expressions can mean essentially the same thing, for example `A v B` and `B v A`.
      (is (= expected-dnf (dnf expr))))))

(run-tests 'logic.tests)
