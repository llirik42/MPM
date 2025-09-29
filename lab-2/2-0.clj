(defn my-func [x] x)

(defn integral [f]
  (let [h 0.1]
    (letfn [(_int-k [k acc]
                    (if (> k 0)
                      (let [f-prev (f (* h (- k 1)))
                            f-cur (f (* h k))
                            coeff (/ h 2)
                            delta (* coeff (+ f-prev f-cur))]
                        (recur (- k 1) (+ acc delta)))
                      acc))
            (_int-x [x]
                    (let [k (Math/round (/ x h))]
                      (_int-k k 0)))
            (_int-x-memo
             (memoize _int-x))]
      _int-x-memo)))


(time ((integral my-func) 10000000))
(time ((integral my-func) 10000000))
(time ((integral my-func) 10000000))
(time ((integral my-func) 10000000))

;; (println (Math/round (/ 10 0.12)))
