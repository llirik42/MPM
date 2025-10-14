(ns logic.utils)

(defn int-or-bool-to-bool
  [v]
  (if (int? v)
    (if (== v 0) false true)
    v))

(defn find-first [pred coll] (first (filter pred coll)))
