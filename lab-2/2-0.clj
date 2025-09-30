(ns lab_2/2_0
  (:require [incanter.charts :as charts]
            [incanter.core :as ic]))
(def h 0.01)

(defn f [x] 1)

(defn integral-k
  [k acc]
  (if (> k 0)
    (let [f-prev (f (* h (- k 1)))
          f-cur (f (* h k))
          coeff (/ h 2)
          delta (* coeff (+ f-prev f-cur))]
      (recur (- k 1) (+ acc delta)))
    acc))

(def integral-k-memo (memoize integral-k))

(defn integral
  [x]
  (let [k (Math/round (/ x h))]
    (integral-k-memo k 0)))

;; (println ((integral my-func) 10000000))
(println (integral 1000000))
(time (integral 1000000))
(time (integral 1000000))

;; (println (Math/round (/ 10 0.12)))
