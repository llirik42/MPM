(defn fib []
  (letfn [(slow-fib [f n] (if (<= n 1)
                            n
                            (+ (f f (- n 1)) (f f (- n 2)))))]
    (let [memoized-fib (memoize slow-fib)]
      (fn [n] ((partial memoized-fib memoized-fib) n)))))

(let [memoized-fib (memoize fib)]
  (time ((memoized-fib) 40))
  (time ((memoized-fib) 84))
  (time ((memoized-fib) 85))
  (time ((memoized-fib) 86))
  (time ((memoized-fib) 87))
  (time ((memoized-fib) 88))
  (time ((memoized-fib) 89))
  (time ((memoized-fib) 90))
  (time ((memoized-fib) 91)))
