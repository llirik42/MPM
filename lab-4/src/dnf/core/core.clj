(ns dnf.core.core)

(declare repr)

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

(defn -const-repr
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

(defn -variable-repr
  [v]
  (name (variable-name v)))

(defn args
  [expr]
  (rest expr))

(defn lneg
  [expr]
  (list ::neg expr))

(defn lneg?
  [expr]
  (= (first expr) ::neg))

(defn -lneg-repr
  [expr]
  (str "¬" (repr (first (args expr)))))

(defn land
  [expr & other]
  (cons ::and (cons expr other)))

(defn land?
  [expr]
  (= (first expr) ::and))

(defn -land-repr
  [expr]
  (let [args (args expr)
        reprs (map repr args)
        f (fn [r1 r2] (str r1 " & " r2))]
    (str "(" (reduce f reprs) ")")))

(defn lor
  [expr & other]
  (cons ::or (cons expr other)))

(defn lor?
  [expr]
  (= (first expr) ::or))

(defn -lor-repr
  [expr]
  (let [args (args expr)
        reprs (map repr args)
        f (fn [r1 r2] (str r1 " v " r2))]
    (str "(" (reduce f reprs) ")")))

(defn limpl
  [e1 e2]
  (list ::impl e1 e2))

(defn limpl?
  [expr]
  (= (first expr) ::impl))

(defn -limpl-repr [expr]
  (let [args (args expr)
        a1 (first args)
        a2 (second args)]
    (str "(" (repr a1) " → " (repr a2) ")")))

(defn repr [expr]
  (cond (const? expr) (-const-repr expr)
        (variable? expr) (-variable-repr expr)
        (lneg? expr) (-lneg-repr expr)
        (land? expr) (-land-repr expr)
        (lor? expr) (-lor-repr expr)
        (limpl? expr) (-limpl-repr expr)))
