(ns recursion.2)

(let [n 3
      cs `("a" "b" "c" "d")]

  ; Function takes character and list of strings. Returns: list of strings with given character inserted at the beginning if first character of strings didn't equal it
  (letfn [(_add-single-character
            [string-list char acc]
            (if (> (count string-list) 0)
              (if (= char (str (first (first string-list))))
                (recur (rest string-list) char acc)
                (let [acc-delta (.concat (str char) (first string-list))
                      new-acc (cons acc-delta acc)]
                  (recur (rest string-list) char new-acc)))
              acc))]

    (letfn [(add-single-character
              [string-list char]
              (_add-single-character string-list char `()))]

    ; Function takes characters and list of strings. Returns: list of strings with given characters inserted at the beginning of all strings if first character of strings didn't equal exact character (applies "add-single-character" multiple times)
      (letfn [(_add-multiple-characters
                [string-list chars acc]
                (if (> (count chars) 0)
                  (let [acc-delta (add-single-character string-list (first chars))
                        new-acc (concat acc acc-delta)]
                    (recur string-list (rest chars) new-acc))
                  acc))]

        (letfn [(add-multiple-characters
                  [string-list chars]
                  (_add-multiple-characters string-list chars `()))]

      ; Function solves original problem
          (letfn [(_get-all-sequences
                    [length chars acc]
                    {:pre [(> length 0)]}
                    (if (> length 1)
                      (let [new-acc (add-multiple-characters acc chars)]
                        (recur (dec length) chars new-acc))
                      acc))]

            (letfn [(get-all-sequences
                      [length chars]
                      (_get-all-sequences length chars chars))]

              (= (frequencies (get-all-sequences n cs)) (frequencies `("aba" "abc" "abd" "aca" "acb" "acd" "ada" "adb" "adc" "bab" "bac" "bad" "bca" "bcb" "bcd" "bda" "bdb" "bdc" "cab" "cac" "cad" "cba" "cbc" "cbd" "cda" "cdb" "cdc" "dab" "dac" "dad" "dba" "dbc" "dbd" "dca" "dcb" "dcd"))))))))))
