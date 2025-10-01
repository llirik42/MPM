(ns integration.memo
  (:require [integration.utils :as utils]))

(def antiderivative
  (memoize (fn [f h]
             (letfn [(integral-k
                       [g k1]
                       (if (not (= k1 0))
                         (let [positive? (> k1 0)
                               k2 (if positive? (- k1 1) (+ k1 1))
                               f1 (f (* h k1))
                               f2 (f (* h k2))
                               c (/ h 2)
                               coeff (if positive? c (- c))
                               delta (* coeff (+ f1 f2))]
                           (+ (g g k2) delta))
                         0))]
               (let [memo (memoize integral-k)]
                 (fn [x]
                   (let [k (Math/round (double (/ x h)))]
                     ((partial memo memo) k))))))))

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


(let [i (antiderivative utils/linear 1)]
  (println i)
  (time (i 200))
  (time (i 200))
  (time (i 201))
  (time (i 202))
  (println "_________________"))
