(defn make-fibo [y]
  (let [fib
    (fn [mem-fib x]
      (let [fib2 (fn [a] (mem-fib mem-fib a))]
        (if (<= x 2)
          y
          (+ (fib2 (- x 1)) (fib2 (- x 2))))))
    mem-fib (memoize fib)]

    (partial mem-fib mem-fib)))

(time ((make-fibo 1) 90))
(time ((make-fibo 1) 90))
(time ((make-fibo 1) 90))
(time ((make-fibo 1) 90))
(time ((make-fibo 1) 90))
(time ((make-fibo 1) 90))
(time ((make-fibo 1) 90))
