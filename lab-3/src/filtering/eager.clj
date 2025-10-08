(ns filtering.eager
(:require [criterium.core :as criterium]))

(def cpu-n (.availableProcessors (Runtime/getRuntime)))

(defn -partition
  ([n coll acc]
   (if (= 0 (count coll))
     acc
     (let [delta (take n coll)
           new-acc (concat acc (list delta))]
       (recur n (drop n coll) new-acc))))
  ([n coll] (-partition n coll (list))))


(defn parallel-filter
  [pred coll]
  (let [block-size (int (Math/ceil (/ (count coll) cpu-n)))]
    (->> coll
         (-partition block-size)
         (map #(future (filter pred %)))
         (doall)
         (map deref)
         (reduce concat `()))))
;; (->> (range 100)
;;      (filter #(= 0 (mod % 2)))
;;      (map #(* % %))
;;      (take 10))

(criterium/quick-bench (dorun (parallel-filter even? (range 10000000))))
(criterium/quick-bench (dorun (filter even? (range 10000000))))


;; (println (-partition 5 (range 1001)))
