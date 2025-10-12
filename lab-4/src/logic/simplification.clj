(ns logic.simplification
  (:require [logic.core :refer :all])
  (:require [logic.repr :refer :all])
  (:require [logic.utils :refer :all]))

(declare simplify)

(def -simplification-rules
  (list
   [(fn [expr] (and (land? expr) (find-first const? (args expr)))); Логическое И, в котором есть константы
    (fn [expr] (let [args (args expr)
                     value-of-const (const-value (find-first const? args))]
                 (if value-of-const
                   (let [other-args (remove #(= (const 1) %) args)] ; ; const = true удаляем первый true
                     (simplify (apply land other-args)))
                   (const 0))))] ; const = false 
   
   [land? (fn [expr] expr)] ; Логическое И. Возвращаем без изменений

   [lor?; ИЛИ
    (fn [expr] expr)]))

(defn simplify [expr]
  ((some
    (fn [rule]
      (if ((first rule) expr)
        (second rule)
        false))
    -simplification-rules)
   expr))

(let [t (const 1)
      f (const 0)
      a (variable ::A)
      b (variable ::B)
      c (variable ::C)
      d (variable ::D)
      v (land a a)]
  (println (repr v))
  (println (repr (simplify v))))
