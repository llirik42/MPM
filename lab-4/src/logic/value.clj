(ns logic.value
  (:require [logic.core :refer :all]
            [logic.utils :refer [int-bool-to-bool]]))

(declare value)

(def -value-rules
  (list
   [const?
    (fn [expr ctx]
      (const-value expr))]
   [variable?
    (fn [expr ctx]
      (let [name (variable-name expr)
            v (int-bool-to-bool (ctx name))]
        (if (nil? v)
          (throw (Exception. (str "Unknown variable " name)))
          v)))]
   [lneg?
    (fn [expr ctx]
      (let [arg (first (args expr))]
        (not (value arg ctx))))]
   [land?
    (fn [expr ctx]
      (let [args (args expr)
            values (map #(value % ctx) args)]
        (reduce #(and %1 %2) true values)))]
   [lor?
    (fn [expr ctx]
      (let [args (args expr)
            values (map #(value % ctx) args)]
        (reduce #(or %1 %2) false values)))]
   [limpl?
    (fn [expr ctx]
      (let [[a1 a2] (args expr)
            v1 (value a1 ctx)
            v2 (value a2 ctx)]
        (or (not v1) v2)))]))

(defn value [expr ctx]
  ((some
    (fn [rule]
      (if ((first rule) expr)
        (second rule)
        false))
    -value-rules)
   expr ctx))
