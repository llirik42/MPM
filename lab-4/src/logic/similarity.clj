(ns logic.similarity
  (:require [logic.core :refer :all])
  (:require [logic.utils :refer :all])
  (:require [logic.repr :refer :all]))

(declare same?)

(def -similarity-rules
  (list
   [(fn [e1 e2] (and (const? e1) (const? e2))) (fn [e1 e2] (= (const-value e1) (const-value e2)))] ; проверка констант на совпадение

   [(fn [e1 e2] (and (variable? e1) (variable? e2))) (fn [e1 e2] (= (variable-name e1) (variable-name e2)))]  ; проверка переменных на совпадение

   [(fn [e1 e2] (and (lneg? e1) (lneg? e2))) (fn [e1 e2] (same? (first-arg e1) (first-arg e2)))]  ; проверка отрицаний на совпадение

   [(fn [e1 e2] (and (land? e1) (land? e2))) ; проверка конъюнкций на совпадение
    (fn [e1 e2] (let [args1 (apply hash-set (args e1))
                      args2 (apply hash-set (args e2))]
                  (if (= (count args1) (count args2))
                    (let [similarities (map same? args1 args2)]
                      (reduce #(and %1 %2) true similarities))
                    false)))]

   [(fn [e1 e2] (and (lor? e1) (lor? e2))) ; проверка дизъюнкций на совпадение
    (fn [e1 e2] (let [args1 (apply hash-set (args e1))
                      args2 (apply hash-set (args e2))]
                  (if (= (count args1) (count args2))
                    (let [similarities (map same? args1 args2)]
                      (reduce #(and %1 %2) true similarities))
                    false)))]

   [(fn [e1 e2] (and (limpl? e1) (limpl? e2))) ; проверка импликаций на совпадение
    (fn [e1 e2] (let [a11 (first-arg e1)
                      a12 (second-arg e1)
                      a21 (first-arg e2)
                      a22 (second-arg e2)]
                  (same? lor1 lor2)))]

   [(fn [e1 e2] true) (fn [e1 e2] false)] ; все предыдущие правила не сработали, значит выражения точно не совпадают
   ))

(defn same? [e1 e2]
  (let [rule (find-first #((first %) e1 e2) -similarity-rules)]
    ((second rule) e1 e2)))

(let [t (const 1)
      f (const 0)
      a (variable ::A)
      b (variable ::B)
      c (variable ::C)
      d (variable ::D)
      v (land a a)]
  (println (repr (limpl a b)) (repr (limpl (lneg b) (lneg a))))
  (println (same? (limpl a b) (limpl (lneg b) (lneg a))))
  )
