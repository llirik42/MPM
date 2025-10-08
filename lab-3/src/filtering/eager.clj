(ns filtering.eager)

partition

(defn -partition
  ([n coll]
   (lazy-seq
    (when-let [_ (seq coll)]
      (cons (take n coll) (-partition n (drop n coll)))))))

(def naturals
  (lazy-seq
   (cons 1 (map inc naturals)))) 

;; (defn pfilter
;;   ([pred coll block-size]
;;    ())
;;   ([pred coll] (pfilter pred coll 32)))


(println (nth (-partition 32 naturals) 0))
(println (nth (-partition 32 naturals) 1))
(println (nth (-partition 32 naturals) 2))
(println (nth (-partition 32 naturals) 3))
(println (nth (-partition 32 naturals) 4))
