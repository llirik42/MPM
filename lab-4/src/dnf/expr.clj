(ns dnf.expr)

(defn -integer-to-boolean
  [i]
  {:pre [(int? i)]}
  (if (== i 0) false true))

(defn constant
  [value]
  {:pre [(or (boolean? value) (int? value))]}
  (let [v (if (int? value)
            (-integer-to-boolean value)
            value)]
    (list ::constant v)))

(defn constant?
  [expr]
  (= (first expr) ::constant))

(defn constant-value
  [c]
  (second c))

(defn variable
  [name]
  {:pre [(keyword? name)]}
  (list ::variable name))

(defn variable?
  [expr]
  (= (first expr) ::variable))

(defn variable-name
  [v]
  (second v))
