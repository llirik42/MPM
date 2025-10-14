(ns logic.core
  (:require [logic.utils :refer [int-or-bool-to-bool]]))

(defn const
  "Creates a new expression with constant type. The accepted value can be either true/false or 0/1."
  [value]
  (list ::const (int-or-bool-to-bool value)))

(defn const?
  "Returns true if then given expression has a constant type."
  [expr]
  (= (first expr) ::const))

(defn const-value
  "Returns value (true/false) of the given constant expression."
  [c]
  (second c))

(defn variable
  "Creates a new expression with variable type. The accepted name must be keyword."
  [name]
  {:pre [(keyword? name)]}
  (list ::variable name))

(defn variable?
  "Returns true if the given expression has a variable type."
  [expr]
  (= (first expr) ::variable))

(defn variable-name
  "Returns name of the given expression with variable type."
  [v]
  (second v))

(defn lneg
  "Creates a new expression with a negation type. Result will be a negation of the given expression."
  [expr]
  (list ::neg expr))

(defn lneg?
  "Returns true if the given expression has a negation type."
  [expr]
  (= (first expr) ::neg))

(defn land
  "Creates a new expression with a conjuction type (and). Result will be a conjuction of the all given expressions."
  [expr & other]
  (cons ::and (cons expr other)))

(defn land?
  "Returns true if the given expression has a conjuction type."
  [expr]
  (= (first expr) ::and))

(defn lor
  "Creates a new expression with a disjunction type (or). Result will be a disjunction of the all given expressions."
  [expr & other]
  (cons ::or (cons expr other)))

(defn lor?
  "Returns true if the given expression has a disjunction type."
  [expr]
  (= (first expr) ::or))

(defn limpl
  "Creates a new expression with an implication type. Result will be an implication of the given expressions."
  [e1 e2]
  (list ::impl e1 e2))

(defn limpl?
  "Returns true if the given expression has an implication type."
  [expr]
  (= (first expr) ::impl))

(defn args
  "Returns arguments of the given expression. For example, if the given expression is (A v B v C), then it will return list of expressions A, B, C."
  [expr]
  (rest expr))

(defn first-arg
  "Returns the first argument of the given expression. For example, if the given expression is (A v B), then it will return expression A. Function doesn't check whether the expression has arguments or not (expressions with constant and variable types don't have arguments)."
  [expr]
  (first (rest expr)))

(defn second-arg
  "Returns the second argument of the given expression. For example, if the given expression is (A v B), then it will return expression B. Function doesn't check whether the expression has arguments or not (expressions with constant and variable types don't have arguments)."
  [expr]
  (second (rest expr)))
