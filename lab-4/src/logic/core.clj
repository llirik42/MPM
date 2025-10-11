(ns logic.core)

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

(defn args
  [expr]
  (rest expr))

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
