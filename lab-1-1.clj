(let [n 2
      cs `("a" "b" "c")]
  (letfn [(add-single-character
            [string-list char]
            (if (> (count string-list) 0)
              (if (= char (str (first (first string-list))))
                (add-single-character (rest string-list) char)
                (cons (.concat (str char) (first string-list))
                      (add-single-character (rest string-list) char)))
              (list)))]
    (letfn [(add-multiple-characters
              [string-list chars acc]
              (if (> (count chars) 0)
                (add-multiple-characters string-list (rest chars) (concat acc (add-single-character string-list (first chars))))
                acc))]
      (letfn [(chars-to-strings
                [chars acc]
                (if (> (count chars) 0)
                  (chars-to-strings (rest chars) (concat acc (list (str (first chars)))))
                  acc))]
        (letfn [(get-all-sequences
                  [length chars]
                  {:pre [(> length 0)]} 
                  (if (> length 1)
                    (add-multiple-characters (get-all-sequences (dec length) chars) chars `())
                    chars))]
          (println (get-all-sequences n cs)))))))
