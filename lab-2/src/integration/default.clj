(ns integration.default)

(def antiderivative
  (fn [f h]
    (letfn [(integral-k
              [k1]
              (if (not (= k1 0))
                (let [positive? (> k1 0)
                      k2 (if positive? (- k1 1) (+ k1 1))
                      f1 (f (* h k1))
                      f2 (f (* h k2))
                      c (/ h 2)
                      coeff (if positive? c (- c))
                      delta (* coeff (+ f1 f2))]
                  (+ (integral-k k2) delta))
                0))]
      (fn [x]
        (let [k (Math/round (double (/ x h)))]
          (integral-k k))))))
