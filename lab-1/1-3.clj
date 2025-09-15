(letfn [(my-map
          [f coll]
          (reduce
           (fn [acc el]
             (let [acc-delta (f el)]
               (concat acc (list acc-delta))))
           `()
           coll))]

  (letfn [(_my-filter
            [pred coll acc]
            (if (> (count coll) 0)
              (if (pred (first coll))
                (recur pred (rest coll) (concat acc (list (first coll))))
                (recur pred (rest coll) acc))
              acc))]

    (letfn [(my-filter
              [pred coll]
              (_my-filter pred coll `()))]
      (println (my-map dec `(1 2 3 4 5)))
      (println (my-map inc `(1)))
      (println (my-map inc `()))
      (println (my-filter even? `(8 1 2 3 4 5 6))))))
