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

pmap

(defn pfilter
  ([pred partitions]
   (lazy-seq
    (when-let [s (seq (take 8 partitions))]
      (concat (->> s
                   (map #(future (doall (filter pred %))))
                   (doall)
                   (map deref)
                   (doall)
                   (reduce concat)) (pfilter pred (drop 8 partitions))))))
  ([pred coll block-size] (pfilter pred (-partition block-size coll))))

(defn my-pred [_]
  (Thread/sleep 100)
  true)

(time (doall (pfilter my-pred (range 16) 1)))
(time (doall (filter my-pred (range 16))))




;; (time (pfilter even? naturals 32))
;; (time (nth (pfilter even? naturals 32) 1000000))
