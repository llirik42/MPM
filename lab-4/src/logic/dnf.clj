(ns logic.dnf
  (:require [logic.core :refer :all])
  (:require [logic.repr :refer :all]))

(declare dnf)

(defn flatten-or [expr] (let [or-args (args expr)
                              dnf-args (map dnf or-args)
                              new-args (reduce (fn [acc v]
                                                 (if (lor? v)
                                                   (concat acc (args v))
                                                   (concat acc (list v))))
                                               (list)
                                               dnf-args)]
                          (apply lor new-args)))

(defn flatten-and [expr] (let [and-args (args expr) 
                              new-args (reduce (fn [acc v]
                                                 (if (land? v)
                                                   (concat acc (args v))
                                                   (concat acc (list v))))
                                               (list)
                                               and-args)]
                          (apply land new-args)))

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
   
   ; TODO: пофиксить (dnf не там, где надо)
   [(fn [expr] (and (lneg? expr) (land? (first-arg expr)))); Отрицание И (закон Де-моргана)
    (fn [expr] (let [and-args (args (first-arg expr))
                     mapped-and-args (map #(dnf (lneg %)) and-args)]
                 (dnf (apply lor mapped-and-args))))] ; Закон Де-моргана
   
   ; TODO: пофиксить (dnf не там, где надо)
   [(fn [expr] (and (lneg? expr) (lor? (first-arg expr)))); Отрицание ИЛИ (закон Де-моргана)
    (fn [expr] (let [or-args (args (first-arg expr))
                     mapped-or-args (map #(dnf (lneg %)) or-args)]
                 (dnf (apply land mapped-or-args))))] ; Закон Де-моргана
   
   ; TODO: пофиксить (dnf не там, где надо)
   [(fn [expr] (and (lneg? expr) (limpl? (first-arg expr)))); Отрицание импликации
    (fn [expr] (let [arg (first-arg expr)] ; arg is implication
                 (dnf (lneg (dnf arg)))))]
   
   [(fn [expr] (and (land? expr) (== 1 (count (args expr))))) ; Вырожденный случай: конъюнкция одного аргумента
    (fn [expr] (dnf (first-arg expr)))]
   
   [(fn [expr] (and (== 2 (count (args expr))) (land? expr) (lor? (second-arg expr)))); A & (B v C v D v ...)
    (fn [expr] (let [arg1 (first-arg expr)
                     arg2 (second-arg expr)
                     or-args (args arg2)
                     ands (map #(land arg1 %) or-args)] ; TODO: тут нет dnf
                 (apply lor ands)))]

   [(fn [expr] (and (== 2 (count (args expr))) (land? expr) (lor? (first-arg expr)))); (B v C v D v ...) & A
    (fn [expr] (let [arg1 (first-arg expr)
                     arg2 (second-arg expr)
                     or-args (args arg1)
                     ands (map #(land % arg2) or-args)] ; TODO: тут нет dnf
                 (apply lor ands)))] 
   
   [land? ; все остальные случаи для конъюнкции
    (fn [expr] (if (> (count (args expr)) 2)
                 (let [args (args expr)
                       f (first args)
                       r (rest args)
                       tmp (land (dnf f) (dnf (apply land r)))] ; TODO: назвать нормально 
                   (dnf (flatten-and tmp)))
                 (land (dnf (first-arg expr)) (dnf (second-arg expr)))))] ; точно 2 аргумента

   [(fn [expr] (and (lor? expr) (== 1 (count (args expr))))) ; Вырожденный случай: дизъюнкция одного аргумента
    (fn [expr] (dnf (first-arg expr)))]

   [lor? ; Остальные случаи для ИЛИ
    (fn [expr] (flatten-or expr))]

   [limpl?; Импликация
    (fn [expr] (let [a1 (first-arg expr)
                     a2 (second-arg expr)]
                 (dnf (lor (lneg a1) a2))))]))

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
      v (land a (lor b c) d)
      v2 (land a (land b c))]
  ; НЕ РАБОТАЕТ
  (println (repr v2))
  (println "----")
  (println (repr (dnf v2))))
