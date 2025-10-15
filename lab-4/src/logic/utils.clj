(ns logic.utils)

(defn int-or-bool-to-bool
  "Function either takes an integer and converts it to Boolean (0 is false, the rest is true), or it takes a boolean and returns it unchanged."
  [v]
  (if (int? v)
    (if (== v 0) false true)
    v))

(defn find-first
  "Returns the first element of the collection that satisfies the given predicate."
  [pred coll]
  (first (filter pred coll)))
