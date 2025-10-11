(ns logic.core
  (:require [logic.utils :refer [int-bool-to-bool]]))

(defn const
  [value]
  (list ::const (int-bool-to-bool value)))

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

(defn args
  [expr]
  (rest expr))

(defn first-arg
  [expr]
  (first (rest expr)))
