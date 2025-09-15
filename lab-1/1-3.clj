(letfn [(_my-map
          [f coll acc]
          (if (> (count coll) 0)
            (let [acc-delta (f (first coll))
                  new-acc (concat acc (list acc-delta))]
              (recur f (rest coll) new-acc))
            acc))]
  
  (letfn [(my-map [f coll] (_my-map f coll `()))]

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
        (println (my-filter even? `(8 1 2 3 4 5 6)))))))
