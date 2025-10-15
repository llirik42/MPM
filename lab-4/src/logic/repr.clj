(ns logic.repr
  (:require [logic.core :refer :all]
            [logic.utils :refer [find-first]]))

(declare repr)

(def -repr-rules
  (list
   ;; Handling the constant.
   [const?
    (fn [expr] (if (const-value expr) "1" "0"))]

   ;; Handling the variable.
   [variable?
    (fn [expr] (name (variable-name expr)))]

   ;; Handling the negation.
   [lneg?
    (fn [expr] (str "¬" (repr (first (args expr)))))]

   ;; Handling the conjuction.
   [land?
    (fn [expr]
      (let [reprs (map repr (args expr))
            f (fn [r1 r2] (str r1 " & " r2))]
        (str "(" (reduce f reprs) ")")))]

   ;; Handling the disjunction.
   [lor?
    (fn [expr]
      (let [reprs (map repr (args expr))
            f (fn [r1 r2] (str r1 " v " r2))]
        (str "(" (reduce f reprs) ")")))]

   ;; Handling the implication.
   [limpl?
    (fn [expr]
      (let [args (args expr)
            a1 (first args)
            a2 (second args)]
        (str "(" (repr a1) " → " (repr a2) ")")))]))

(defn repr
  "Returns string representation of the given expression. Function goes through all the subexpressions recursively."
  [expr]
  (let [rule (find-first #((first %) expr) -repr-rules)]
    ((second rule) expr)))
