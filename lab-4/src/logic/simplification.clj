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
                     (if (empty? other-args)
                       (const 1) ; Стало пусто после того, как удалили все 1, значит было что-то вроде 1 & 1 & 1 & ...
                       (-simplify (apply land other-args))))
                   (const 0))))] ; const = false

   [land? (fn [expr] (let [expr-args (args expr)
                           simplified-args (map -simplify expr-args)]
                       (apply land simplified-args)))]

   [(fn [expr] (and (lor? expr) (== 1 (count (args expr))))) ; Вырожденный случай: дизъюнкция одного аргумента
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

(defn simplify [expr] 
  ;(println "Before: " (repr expr) "    After: " (repr (-simplify expr)))
  (-simplify expr))
