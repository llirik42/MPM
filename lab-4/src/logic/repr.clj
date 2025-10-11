(ns logic.repr
  (:require [logic.core :refer :all]))

(declare repr)

(def repr-rules
  (list
   [const? (fn [expr] (if (const-value expr) "1" "0"))]
   [variable? (fn [expr] (name (variable-name expr)))]
   [lneg? (fn [expr] (str "¬" (repr (first (args expr)))))]
   [land? (fn [expr]
            (let [args (args expr)
                  reprs (map repr args)
                  f (fn [r1 r2] (str r1 " & " r2))]
              (str "(" (reduce f reprs) ")")))]
   [lor? (fn [expr]
           (let [args (args expr)
                 reprs (map repr args)
                 f (fn [r1 r2] (str r1 " v " r2))]
             (str "(" (reduce f reprs) ")")))]
   [limpl? (fn [expr]
             (let [args (args expr)
                   a1 (first args)
                   a2 (second args)]
               (str "(" (repr a1) " → " (repr a2) ")")))]))

(defn repr [expr]
  ((some (fn [rule]
           (if ((first rule) expr)
             (second rule)
             false))
         repr-rules)
   expr))
