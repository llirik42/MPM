(ns logic.simplification
  (:require [logic.core :refer :all])
  (:require [logic.repr :refer :all])
  (:require [logic.utils :refer :all]))

(declare -simplify)

(def -simplification-rules
  (list
   [(fn [expr] (and (land? expr) (== 1 (count (args expr))))) ; Вырожденный случай: конъюнкция одного аргумента
    (fn [expr] (-simplify (first-arg expr)))]

   [(fn [expr] (and (land? expr) (find-first const? (args expr)))); Логическое И, в котором есть константы
    (fn [expr] (let [args (args expr)
                     value-of-const (const-value (find-first const? args))]
                 (if value-of-const
                   (let [other-args (remove #(= (const 1) %) args)] ; ; const = true удаляем первый true
                     (-simplify (apply land other-args)))
                   (const 0))))] ; const = false

   [land? (fn [expr] (let [expr-args (args expr)
                           simplified-args (map -simplify expr-args)]
                       (apply land simplified-args)))]

   [(fn [expr] (and (lor? expr) (== 1 (count (args expr))))) ; Вырожденный случай: дизъюнкция одного аргумента
    (fn [expr] (-simplify (first-arg expr)))]

   [(fn [expr] (and (lor? expr) (find-first const? (args expr)))); Логическое ИЛИ, в котором есть константы
    (fn [expr] (let [args (args expr)
                     value-of-const (const-value (find-first const? args))]
                 (if value-of-const
                   (let [other-args (remove #(= (const 0) %) args)] ; ; const = false удаляем первый false
                     (-simplify (apply lor other-args)))
                   (const 1))))] ; const = true

   [lor? (fn [expr] (let [expr-args (args expr)
                           simplified-args (map -simplify expr-args)]
                       (apply lor simplified-args)))]

   [(fn [expr] true) (fn [expr] expr)])) ; Все остальные случаи

(defn -simplify [expr]
  ((some (fn [rule]
            (if ((first rule) expr)
              (second rule)
              false))
          -simplification-rules)
    expr))

(defn simplify [expr] 
  (println "Before: " (repr expr) "    After: " (repr (-simplify expr)))
  (-simplify expr))
