(defn linear [x] (* 2 x))

(defn quadratic [x] (* 3 x x))

(defn integral
  [f h]
  (letfn [(integral-k
            [g k]
            (if (> k 0)
              (let [f-prev (f (* h (- k 1)))
                    f-cur (f (* h k))
                    coeff (/ h 2)
                    delta (* coeff (+ f-prev f-cur))]
                (+ (g g (- k 1)) delta))
              0))]
    (let [memo (memoize integral-k)]
      (fn [x] ((partial memo memo) x)))))

(def m (memoize integral))

(let [i (m quadratic 1)]
  (println i)
  (time (i 200))
  (time (i 200))
  (time (i 201))
  (time (i 202))
  (println "_________________"))

(let [i (m quadratic 1)]
  (println i)
  (time (i 200))
  (time (i 200))
  (time (i 201))
  (time (i 202))
  (println "_________________"))

(let [i (m linear 1)]
  (println i)
  (time (i 200))
  (time (i 200))
  (time (i 201))
  (time (i 202))
  (println "_________________"))
