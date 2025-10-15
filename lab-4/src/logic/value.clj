(ns logic.value
  (:require [logic.core :refer :all]
            [logic.utils :refer [int-or-bool-to-bool find-first]]))

(declare value)

(def -value-rules
  (list
   ;; Handling the constant.
   [const?
    (fn [expr ctx]
      (const-value expr))]

   ;; Handling the variable.
   [variable?
    (fn [expr ctx]
      (let [name (variable-name expr)
            v (int-or-bool-to-bool (ctx name))]
        (if (nil? v)
          (throw (Exception. (str "Unknown variable " name)))
          v)))]

   ;; Handling the negation.
   [lneg?
    (fn [expr ctx]
      (let [arg (first (args expr))]
        (not (value arg ctx))))]

   ;; Handling the conjuction.
   [land?
    (fn [expr ctx]
      (let [args-values (map #(value % ctx) (args expr))]
        (reduce #(and %1 %2) true args-values)))]

   ;; Handling the disjunction.
   [lor?
    (fn [expr ctx]
      (let [args-values (map #(value % ctx) (args expr))]
        (reduce #(or %1 %2) false args-values)))]

   ;; Handling the implication.
   [limpl?
    (fn [expr ctx]
      (let [[a1 a2] (args expr)
            v1 (value a1 ctx)
            v2 (value a2 ctx)]
        (or (not v1) v2)))]))

(defn value
  "Returns value of the given expression (true/false) in the given context. The context must be a dictionary, where the key is a name of variable and the value is the variable's value (0/1 or true/false). Function goes through all the subexpressions recursively."
  [expr ctx]
  (let [rule (find-first #((first %) expr) -value-rules)]
    ((second rule) expr ctx)))
