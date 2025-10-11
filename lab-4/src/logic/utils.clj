(ns logic.utils)

(defn int-bool-to-bool
  [v]
  {:pre [(or (boolean? v) (int? v))]}
  (if (int? v)
    (if (== v 0) false true)
    v))
