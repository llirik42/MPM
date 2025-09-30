(ns project.1)

(let [n 3
      cs `("a" "b" "c" "d")]
  ; Function takes character and list of strings. Returns: list of strings with given character inserted at the beginning if first character of strings didn't equal it
  (letfn [(add-single-character
            [string-list char]
            (if (> (count string-list) 0)
              (if (= char (str (first (first string-list))))
                (add-single-character (rest string-list) char)
                (cons (.concat (str char) (first string-list))
                      (add-single-character (rest string-list) char)))
              (list)))]

    ; Function takes characters and list of strings. Returns: list of strings with given characters inserted at the beginning of all strings if first character of strings didn't equal exact character (applies "add-single-character" multiple times)
    (letfn [(add-multiple-characters
              [string-list chars acc]
              (if (> (count chars) 0)
                (add-multiple-characters string-list (rest chars) (concat acc (add-single-character string-list (first chars))))
                acc))]

      ; Function solves original problem
      (letfn [(get-all-sequences
                [length chars]
                {:pre [(> length 0)]}
                (if (> length 1)
                  (add-multiple-characters (get-all-sequences (dec length) chars) chars `())
                  chars))]
        (get-all-sequences n cs)))))
