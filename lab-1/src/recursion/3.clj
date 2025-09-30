(ns recursion.3)

(letfn [(my-map
          [f coll]
          (reduce
           (fn [acc el]
             (let [acc-delta (f el)]
               (concat acc (list acc-delta))))
           `()
           coll))]

  (letfn [(my-filter
            [pred coll]
            (reduce
             (fn [acc el]
               (if (pred el)
                 (concat acc (list el))
                 acc))
             `()
             coll))]
    (println (my-map dec `(1 2 3 4 5)))
    (println (my-map inc `(1)))
    (println (my-map inc `()))
    (println (my-filter even? `(8 1 2 3 4 5 6)))
    (println (my-filter odd? `(8 1 2 3 4 5 6)))
    (println (my-filter odd? `()))))
