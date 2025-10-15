(ns logic.dnf
  (:require [logic.core :refer :all])
  (:require [logic.repr :refer :all])
  (:require [logic.utils :refer [find-first]]))

(declare -dnf)

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

(defn flatten-or [expr] (let [or-args (args expr)
                              dnf-args (map -dnf or-args)
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
                 (-dnf neg-arg)))] ; Возврат выражения без отрицания (Двойное отрицание)

   ; TODO: пофиксить (dnf не там, где надо)
   [(fn [expr] (and (lneg? expr) (land? (first-arg expr)))); Отрицание И (закон Де-моргана)
    (fn [expr] (let [and-args (args (first-arg expr))
                     mapped-and-args (map #(-dnf (lneg %)) and-args)]
                 (-dnf (apply lor mapped-and-args))))] ; Закон Де-моргана

   ; TODO: пофиксить (dnf не там, где надо)
   [(fn [expr] (and (lneg? expr) (lor? (first-arg expr)))); Отрицание ИЛИ (закон Де-моргана)
    (fn [expr] (let [or-args (args (first-arg expr))
                     mapped-or-args (map #(-dnf (lneg %)) or-args)]
                 (-dnf (apply land mapped-or-args))))] ; Закон Де-моргана

   ; TODO: пофиксить (dnf не там, где надо)
   [(fn [expr] (and (lneg? expr) (limpl? (first-arg expr)))); Отрицание импликации
    (fn [expr] (let [arg (first-arg expr)] ; arg is implication
                 (-dnf (lneg (-dnf arg)))))]
   
   [(fn [expr] (and (== 2 (count (args expr))) (land? expr) (lor? (second-arg expr)))); A & (B v C v D v ...)
    (fn [expr] (let [arg1 (first-arg expr)
                     arg2 (second-arg expr)
                     or-args (args arg2)
                     ands (map #(-dnf (land arg1 %)) or-args)]
                 (-dnf (apply lor ands))))]

   [(fn [expr] (and (== 2 (count (args expr))) (land? expr) (lor? (first-arg expr)))); (B v C v D v ...) & A
    (fn [expr] (let [arg1 (first-arg expr)
                     arg2 (second-arg expr)
                     or-args (args arg1)
                     ands (map #(-dnf (land % arg2)) or-args)]
                 (-dnf (apply lor ands))))]

   [(fn [expr] (and (== 2 (count (args expr))) (land? expr))) ; Конъюнкция от двух аргументов
    (fn [expr] (let [arg1 (-dnf (first-arg expr))
                     arg2 (-dnf (second-arg expr))
                     l1 (land? arg1)
                     l2 (land? arg2)
                     changed1 (not (= arg1 (first-arg expr)))
                     changed2 (not (= arg2 (second-arg expr)))
                     res (cond
                           (and l1 l2) (let [args1 (args arg1)
                                             args2 (args arg2)]
                                         (apply land (concat args1 args2)))
                           
                           (and l1 (not l2)) (let [args1 (args arg1)
                                                   args2 (list arg2)]
                                               (apply land (concat args1 args2)))
                           
                           (and (not l1) l2) (let [args1 (list arg1)
                                                   args2 (args arg2)]
                                               (apply land (concat args1 args2)))
                           
                           (and (not l1) (not l2)) (if (or (= (lneg arg1) arg2) (= (lneg arg2) arg1))
                                                     (const 0)
                                                     (land arg1 arg2)))]
                 (if (or changed1 changed2)
                   (-dnf res)
                   res)
                 ))]

   [land? ; все остальные случаи для конъюнкции
    (fn [expr] (let [args (args expr)
                     f (first args)
                     r (rest args)
                     tmp (-dnf (land (-dnf f) (-dnf (apply land r))))] ; tmp - вложенная конъюнкция, при этом у конъюнкций не более двух аргументов  
                 (if (land? tmp) ; После преобразования к ДНФ, конъюнкция может стать дизъюнкцией с помощью применения закона Де-Моргана 
                   (flatten-and tmp)
                   tmp)))]
   
   [lor? ; Остальные случаи для ИЛИ
    (fn [expr] (flatten-or expr))]

   [limpl?; Импликация
    (fn [expr] (let [a1 (first-arg expr)
                     a2 (second-arg expr)]
                 (-dnf (lor (lneg a1) a2))))]))



(defn -dnf [expr]
  (let [rule (find-first #((first %) expr) -dnf-rules)]
    ((second rule) expr)))
   

(defn dnf [expr] 
  (let [expr-dnf (-dnf expr)]
    ;(println (repr expr-dnf))
    (simplify expr-dnf)))
