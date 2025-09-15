(let [n 3
      cs `("a" "b" "c" "d")]

  ; Function takes character and list of strings. Returns: list of strings with given character inserted at the beginning if first character of strings didn't equal it
  (letfn [(add-single-character
            [string-list char]
            (letfn [(pred [el] (not (= char (str (first el)))))]
              (let [tmp (filter pred string-list)]
                (map
                 (fn [el] (.concat char el))
                 tmp))))]

    ; Function takes characters and list of strings. Returns: list of strings with given characters inserted at the beginning of all strings if first character of strings didn't equal exact character (applies "add-single-character" multiple times)
    (letfn [(add-multiple-characters
              [string-list chars]
              (letfn [(f
                        [el]
                        (add-single-character string-list el))]
                (let [tmp (map f chars)]
                  (reduce concat tmp))))]

      ; Function solves original problem
      (letfn [(get-all-sequences
                [length chars]
                {:pre [(> length 0)]}
                (let [r (range length)
                      tmp-coll (map (fn [_] chars) r)]
                  (reduce add-multiple-characters tmp-coll)))]

        (= (frequencies (get-all-sequences n cs)) (frequencies `("aba" "abc" "abd" "aca" "acb" "acd" "ada" "adb" "adc" "bab" "bac" "bad" "bca" "bcb" "bcd" "bda" "bdb" "bdc" "cab" "cac" "cad" "cba" "cbc" "cbd" "cda" "cdb" "cdc" "dab" "dac" "dad" "dba" "dbc" "dbd" "dca" "dcb" "dcd")))))))
