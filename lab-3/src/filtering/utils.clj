(ns filtering.utils)

(def n-cpu (.availableProcessors (Runtime/getRuntime)))


(def naturals
  (lazy-seq
   (cons 1 (map inc naturals))))

(defn heavy-pred [_]
  (Thread/sleep 1)
  true)
