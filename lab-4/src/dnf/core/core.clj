(ns dnf.core.core)

(defn -integer-to-boolean
  [i]
  {:pre [(int? i)]}
  (if (== i 0) false true))

(defn const
  [value]
  {:pre [(or (boolean? value) (int? value))]}
  (let [v (if (int? value)
            (-integer-to-boolean value)
            value)]
    (list ::const v)))

(defn const?
  [expr]
  (= (first expr) ::const))

(defn const-value
  [c]
  (second c))

(defn const-repr
  [c]
  (if (const-value c) "1" "0"))

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

(defn variable-repr [v] (name (variable-name v)))

(defn lneg
  [expr]
  (list ::neg expr))

(defn lneg?
  [expr]
  (= (first expr) ::neg))

(defn land
  [expr & other]
  (cons ::and (cons expr other)))

(defn land?
  [expr]
  (= (first expr) ::and))

(defn lor
  [expr & other]
  (cons ::or (cons expr other)))

(defn lor?
  [expr]
  (= (first expr) ::or))

(defn limpl
  [e1 e2]
  (list ::impl e1 e2))

(defn limpl?
  [expr]
  (= (first expr) ::impl))

(defn args [expr]
  (rest expr))
