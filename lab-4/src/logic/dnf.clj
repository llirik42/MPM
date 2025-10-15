(ns logic.dnf
  (:require [logic.core :refer :all])
  (:require [logic.utils :refer [find-first]]))

(declare -simplify)

(def -simplification-rules
  (list
   [(fn [expr] (and (land? expr) (unary? expr))) ; Вырожденный случай: конъюнкция одного аргумента
    (fn [expr] (-simplify (first-arg expr)))]

   [(fn [expr] (and (land? expr) (find-first const? (args expr)))); Логическое И, в котором есть константы
    (fn [expr] (let [args (args expr)
                     value-of-const (const-value (find-first const? args))]
                 (if value-of-const
                   (let [other-args (remove #(= (const 1) %) args)] ; ; const = true удаляем первый true
                     (if (empty? other-args)
                       (const 1) ; Стало пусто после того, как удалили все 1, значит было что-то вроде 1 & 1 & 1 & ...
                       (-simplify (apply land other-args))))
                   (const 0))))] ; const = false

   [land? (fn [expr] (let [expr-args (args expr)
                           simplified-args (map -simplify expr-args)]
                       (apply land simplified-args)))]

   [(fn [expr] (and (lor? expr) (unary? expr))) ; Вырожденный случай: дизъюнкция одного аргумента
    (fn [expr] (-simplify (first-arg expr)))]

   [(fn [expr] (and (lor? expr) (find-first const? (args expr)))); Логическое ИЛИ, в котором есть константы
    (fn [expr] (let [args (args expr)
                     value-of-const (const-value (find-first const? args))]
                 (if (not value-of-const)
                   (let [other-args (remove #(= (const 0) %) args)] ; ; const = false удаляем первый false
                     (if (empty? other-args)
                       (const 0) ; Стало пусто, значит мы удалили нули, значит было что-то вроде 0 v 0 v 0 ...
                       (-simplify (apply lor other-args))))
                   (const 1))))] ; const = true

   [lor? (fn [expr] (let [expr-args (args expr) ; Общий случай для логического или
                          simplified-args (map -simplify expr-args)]
                      (if (not (= expr-args simplified-args))
                        (-simplify (apply lor simplified-args))
                        (apply lor simplified-args))))]

   [(fn [expr] true) (fn [expr] expr)])) ; Все остальные случаи

(defn -simplify [expr]
  (let [rule (find-first #((first %) expr) -simplification-rules)]
    ((second rule) expr)))

(defn -flatten-and1
  "Function unwraps one level of nested conjunctions. Example: `A & (B & (C & (D & E)))` will be `A & B & (C & (D & E))`."
  [expr]
  (let [and-args (args expr)
        new-args (reduce
                  (fn [acc v]
                    (if (land? v)
                      (concat acc (args v))
                      (concat acc (list v))))
                  (list)
                  and-args)]
    (apply land new-args)))

(defn -nest-and1
  "Function converts a conjunction with potentially more than two arguments into a conjunction with two arguments (not recursively) and returns it. Example: `A & B & C & D` will be transformed into `A & (B & C & D)`."
  [expr]
  (let [f (first (args expr))
        r (rest (args expr))]
    (land f (apply land r))))

(declare -dnf)

(defn -flatten-or
  "Function turns nested disjunctions into a single disjunction with a large number of arguments and returns it. Example: `A v (B v (C v D))` will be transformed into `A v B v C v D`."
  [expr]
  (let [or-args (args expr)
        dnf-args (map -dnf or-args) ; Calling of `-dnf` implicitly calls `-flatten-or`.
        new-args (reduce
                  (fn [acc v]
                    (if (lor? v)
                      (concat acc (args v))
                      (concat acc (list v))))
                  (list)
                  dnf-args)]
    (apply lor new-args)))

(def -dnf-rules
  (list

   ;; Handling the constant.
   [const?
    (fn [expr] expr)]

   ;; Handling the variable.
   [variable?
    (fn [expr] expr)]

   ;; Handling the negation of a constant.
   [(fn [expr] (and (lneg? expr) (const? (first-arg expr))))
    (fn [expr]
      (let [arg (first-arg expr)
            v (const-value arg)]
        (const (not v))))]

   ;; Handling the negation of a variable.
   [(fn [expr] (and (lneg? expr) (variable? (first-arg expr))))
    (fn [expr] expr)]

   ;; Handling the negation of a negation. It uses rule `¬(¬A)` ~ `A`.
   [(fn [expr] (and (lneg? expr) (lneg? (first-arg expr))))
    (fn [expr] (first-arg (first-arg expr)))]

   ;; Handling the negation of a conjuction. It uses De Morgan's laws: `¬(A & B)` ~ `¬A v ¬B`.
   [(fn [expr] (and (lneg? expr) (land? (first-arg expr))))
    (fn [expr] (let [and-args (args (first-arg expr))
                     mapped-and-args (map #(lneg %) and-args)]
                 (-dnf (apply lor mapped-and-args))))]

   ;; Handling the negation of a disjunction. It uses De Morgan's laws: `¬(A v B)` ~ `¬A & ¬B`.
   [(fn [expr] (and (lneg? expr) (lor? (first-arg expr))))
    (fn [expr] (let [or-args (args (first-arg expr))
                     mapped-or-args (map #(lneg %) or-args)]
                 (-dnf (apply land mapped-or-args))))]

   ;; Handling the negation of an implication. It recursively uses De Morgan's laws: `¬(A → B)` ~ `¬(¬A v B)` ~ `A & ¬B`.
   [(fn [expr] (and (lneg? expr) (limpl? (first-arg expr))))
    (fn [expr] (let [arg (first-arg expr)]
                 (-dnf (lneg (-dnf arg)))))]

   ;; Handling the conjuction where the second arguments is a disjunction. Uses distributive property: `A & (B v C v ...)` ~ `(A & B) v (A & C) v ...`.
   [(fn [expr] (and (land? expr) (binary? expr) (lor? (second-arg expr))))
    (fn [expr] (let [arg1 (first-arg expr)
                     arg2 (second-arg expr)
                     or-args (args arg2)
                     ands (map #(land arg1 %) or-args)]
                 (-dnf (apply lor ands))))]

   ;; Handling the conjuction where the first arguments is a disjunction. Uses distributive property: `(B v c v ...) & A` ~ `(B & A) v (C & A) v ...`.
   [(fn [expr] (and (land? expr) (binary? expr) (lor? (first-arg expr))))
    (fn [expr] (let [arg1 (first-arg expr)
                     arg2 (second-arg expr)
                     or-args (args arg1)
                     ands (map #(land % arg2) or-args)]
                 (-dnf (apply lor ands))))]

   ;; Handling the conjuction of two arguments.
   [(fn [expr] (and (land? expr) (binary? expr)))
    (fn [expr] (let [arg1 (first-arg expr)
                     arg2 (second-arg expr)
                     arg1-dnf (-dnf arg1)
                     arg2-dnf (-dnf arg2)
                     cond1 (land? arg1-dnf)
                     cond2 (land? arg2-dnf)
                     arg1-changed (not (= arg1-dnf arg1))
                     arg2-changed (not (= arg2-dnf arg2))
                     res (cond
                           ; `E` = `(A1 & B1 & ...) & (A2 & B2 & ...)` ~ `A1 & B1 & ... & A2 & B2 & ...`.
                           (and cond1 cond2)
                           (apply land (concat (args arg1-dnf) (args arg2-dnf)))

                           ; `E` = `(A1 & B1 & ...) & Z` ~ `A1 & B1 & ... & Z`.
                           (and cond1 (not cond2))
                           (apply land (concat (args arg1-dnf) (list arg2-dnf)))

                           ; `E` = `Z & (A1 & B1 & ...)` ~ `Z & A1 & B1 & ...`.
                           (and (not cond1) cond2)
                           (apply land (concat (list arg1-dnf) (args arg2-dnf)))

                           ; `E` = `A & B`.
                           (and (not cond1) (not cond2))
                           (if (or (= (lneg arg1-dnf) arg2-dnf) (= (lneg arg2-dnf) arg1-dnf)) ; TODO: сompare expressions using a special function, rather than using =, since different expressions can mean essentially the same thing, for example `A v B` and `B v A`.
                             (const 0) ; A & ¬A ~ 0
                             (land arg1-dnf arg2-dnf)))]

                 ; At least one argument of the expression changed after the conversion to DNF. This check need to be here to to avoid infinite recursion.
                 (if (or arg1-changed arg2-changed) (-dnf res) res)))]

   ;; Handling other cases of conjuction.
   [land?
    (fn
      [expr]
      (let [nested (-nest-and1 expr)
            nested-dnf (-dnf nested)] ; Calling of `-dnf` implicitly calls `-nest-and1."
            ; Nesting is necessary so that conjunctions with a large number of arguments can be handled as conjunctions with only two arguments using other rules.

        ; This if is needed here, because after conversion to DNF, a conjunction can become a disjunction by applying De Morgan's law.
        (if (land? nested-dnf) (-flatten-and1 nested-dnf) nested-dnf)))]

   ;; Handling other cases of disjunction.
   [lor?
    (fn [expr] (-flatten-or expr))]

   ;; Handling implication. It uses the identity `A → B` ~ `¬A v B`.
   [limpl?
    (fn [expr] (let [a1 (first-arg expr)
                     a2 (second-arg expr)]
                 (-dnf (lor (lneg a1) a2))))]))

(defn -dnf [expr]
  (let [rule (find-first #((first %) expr) -dnf-rules)]
    ((second rule) expr)))

(defn dnf
  "Returns the given expression in disjunctive normal form. Function goes through all the subexpressions recursively."
  [expr]
  (let [expr-dnf (-dnf expr)]
    (-simplify expr-dnf)))
