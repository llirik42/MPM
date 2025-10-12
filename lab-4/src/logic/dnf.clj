(ns logic.dnf
  (:require [logic.core :refer :all])
  (:require [logic.repr :refer :all]))

(declare dnf)

(def -dnf-rules
  (list
   [const? (fn [expr] expr)]
   [variable? (fn [expr] expr)] ; обработка переменной, возвращаем переменную без изменений ; обработка константы, возвращаем константы без изменений
   [(fn [expr] (and (lneg? expr) (const? (first-arg expr)))); Отрицание константы
    (fn [expr]
      (let [arg (first-arg expr)
            v (const-value arg)]
        (const (not v))))] ; вычисления значения новой константы 
   [(fn [expr] (and (lneg? expr) (variable? (first-arg expr)))); Отрицание переменной 
    (fn [expr] expr)] ; возвращаем без изменений
   [(fn [expr] (and (lneg? expr) (lneg? (first-arg expr)))); Отрицание отрицания 
    (fn [expr] (let [neg-arg (first-arg (first-arg expr))]
                 (dnf neg-arg)))] ; Возврат выражения без отрицания (Двойное отрицание)
   [(fn [expr] (and (lneg? expr) (land? (first-arg expr)))); Отрицание И (закон Де-моргана)
    (fn [expr] (let [and-args (args (first-arg expr))
                     mapped-and-args (map #(dnf (lneg %)) and-args)]
                 (dnf (apply lor mapped-and-args))))] ; Закон Де-моргана
   [(fn [expr] (and (lneg? expr) (lor? (first-arg expr)))); Отрицание ИЛИ (закон Де-моргана)
    (fn [expr] (let [or-args (args (first-arg expr))
                     mapped-or-args (map #(dnf (lneg %)) or-args)]
                 (dnf (apply land mapped-or-args))))] ; Закон Де-моргана
   [(fn [expr] (and (lneg? expr) (limpl? (first-arg expr)))); Отрицание импликации
    (fn [expr] (let [arg (first-arg expr)] ; arg is implication
                 (dnf (lneg (dnf arg)))))]
   [land?; И
    (fn [expr] expr)]

   [lor?; ИЛИ
    (fn [expr] expr)]

   [limpl?; Импликация
    (fn [expr] (let [a1 (first-arg expr)
                     a2 (second-arg expr)
                     dnf-neg-a1 (dnf (lneg a1))
                     dnf-a2 (dnf a2)]
                 (dnf (lor dnf-neg-a1 dnf-a2))))]))

(defn dnf [expr]
  ((some
    (fn [rule]
      (if ((first rule) expr)
        (second rule)
        false))
    -dnf-rules)
   expr))

(let [a (variable ::A)
      b (variable ::B)
      c (variable ::C)
      d (variable ::D)
      v (land a b)]
  (println (repr v))
  (println (repr (dnf v))))