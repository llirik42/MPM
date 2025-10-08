(ns filtering.eager)

partition

(defn -partition
  [n coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (cons (take n s) (-partition n (drop n s))))))

(def naturals
  (lazy-seq
   (cons 1 (map inc naturals))))

(defn pfilter
  ([pred partitions]
   (lazy-seq
   (when-let [s (seq (take 8 partitions))]
     (concat (->> s
          (map #(future (filter pred %)))
          (doall)
          (map deref)
          (doall)
          (reduce concat)) (pfilter pred (drop 8 partitions))))))
  ([pred coll block-size] (pfilter pred (-partition block-size coll))))

(time (pfilter even? naturals 32))
(time (nth (pfilter even? naturals 32) 1000000))

;; (println (nth (-partition 32 naturals) 0))
;; (println (nth (-partition 32 naturals) 1))
;; (println (nth (-partition 32 naturals) 2))
;; (println (nth (-partition 32 naturals) 3))
;; (println (nth (-partition 32 naturals) 4))
