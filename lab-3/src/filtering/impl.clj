(ns filtering.impl
  (:require [filtering.utils :refer [n-cpu]]))

(defn -partition
  [n coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (cons (take n s) (-partition n (drop n s))))))

(defn -pfilter
  [pred partitions]
  (lazy-seq
   (let [j n-cpu]
     (when-let [s (seq (take j partitions))]
       (concat (->> s
                    (map #(future (doall (filter pred %))))
                    (doall)
                    (map deref)
                    (doall)
                    (reduce concat)) (-pfilter pred (drop j partitions)))))))

(defn pfilter
  ([pred coll block-size] (-pfilter pred (-partition block-size coll)))
  ([pred coll] (pfilter pred coll 1)))
