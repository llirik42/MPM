(let [n 3
      cs `("a" "b" "c" "d")]
  ; Function takes character and list of strings. Returns: list of strings with given character inserted at the beginning if first character of strings didn't equal it
  (letfn [(add-single-character
            [string-list char acc]
            (if (> (count string-list) 0)
              (if (= char (str (first (first string-list))))
                (add-single-character (rest string-list) char acc)
                (add-single-character (rest string-list) char (cons (.concat (str char) (first string-list)) acc)))
              acc))]

    ; Function takes characters and list of strings. Returns: list of strings with given characters inserted at the beginning of all strings if first character of strings didn't equal exact character (applies "add-single-character" multiple times)
    (letfn [(add-multiple-characters
              [string-list chars acc]
              (if (> (count chars) 0)
                (add-multiple-characters string-list (rest chars) (concat acc (add-single-character string-list (first chars) `())))
                acc))]

      ; Function solves original problem
      (letfn [(get-all-sequences
                [length chars acc]
                {:pre [(> length 0)]}
                (if (> length 1)
                  (get-all-sequences (dec length) chars (add-multiple-characters acc chars `()))
                  acc))]

        (= (frequencies (get-all-sequences n cs cs)) (frequencies `("aba" "abc" "abd" "aca" "acb" "acd" "ada" "adb" "adc" "bab" "bac" "bad" "bca" "bcb" "bcd" "bda" "bdb" "bdc" "cab" "cac" "cad" "cba" "cbc" "cbd" "cda" "cdb" "cdc" "dab" "dac" "dad" "dba" "dbc" "dbd" "dca" "dcb" "dcd")))))))
