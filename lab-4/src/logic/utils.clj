(ns logic.utils)

(defn int-bool-to-bool
  [v]
  {:pre [(or (boolean? v) (int? v))]}
  (if (int? v)
    (if (== v 0) false true)
    v))

(defn find-first [pred coll] (some (fn [el] (if (pred el) el false)) coll))
