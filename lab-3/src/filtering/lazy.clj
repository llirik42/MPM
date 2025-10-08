(ns filtering.lazy)

(defn my-filter [pred coll] (filter pred coll))

(println (when-let [_ (seq (take 5 `()))]
  5))
