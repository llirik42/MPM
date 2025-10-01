(ns integration.lazy
  (:require [integration.utils :as utils]))

(def antiderivative
  (memoize (fn [f h]
             (letfn [(new-pair
                       [k v positive?]
                       (let [k-next (if positive? (+ k 1) (- k 1))
                             f1 (f (* h k))
                             f2 (f (* h k-next))
                             c (/ h 2)
                             coeff (if positive? c (- c))
                             delta (* coeff (+ f1 f2))]
                         [k-next (+ delta v)]))]
               (let [positive-seq (iterate (fn [[k v]] (new-pair k v true)) [0 0])
                     negative-seq (iterate (fn [[k v]] (new-pair k v false)) [0 0])]
                 (fn [x]
                   (let [k (Math/abs (Math/round (double (/ x h))))]
                     (first (rest (if (> x 0)
                                    (nth positive-seq k)
                                    (nth negative-seq k)))))))))))

(let [i (antiderivative utils/quadratic 1)]
  (println i)
  (time (i 200))
  (time (i 200))
  (time (i 201))
  (time (i 202))
  (println "_________________"))

(let [i (antiderivative utils/quadratic 1)]
  (println i)
  (time (i 200))
  (time (i 200))
  (time (i 201))
  (time (i 202))
  (println "_________________"))

(let [i (antiderivative utils/const 1)]
  (println i)
  (time (i -200))
  (time (i -200))
  (time (i -201))
  (time (i -202))
  (println "_________________"))
