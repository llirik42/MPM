(letfn [(_my-map
          [f coll acc]
          (if (> (count coll) 0)
            (let [acc-delta (f (first coll))
                  new-acc (concat acc (list acc-delta))]
              (recur f (rest coll) new-acc))
            acc))]
  
  (letfn [(my-map [f coll] (_my-map f coll `()))]
    
    (println (my-map dec `(1 2 3 4 5)))
    (println (my-map inc `(1)))
    (println (my-map inc ()))))
