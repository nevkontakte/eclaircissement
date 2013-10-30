(ns ru.nsu.ccfit.azhirov.clojure.lab6.atoms
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.expr])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.walk-tree]))

; Constants

(derive ::expr-const ::expr-atom)
(derive ::expr-var ::expr-atom)

(defn constant [value]
  "Create constant expression."
  {:pre [(or (true? value) (false? value))]}
  (list ::expr-const value))

(defn constant? [expr]
  "Check if expression represents a constant."
  {:pre [(expression? expr)]}
  (= ::expr-const (first expr)))

(defn constant-value [expr]
  "Get value of constant."
  {:pre [(constant? expr)]}
  (second expr))

; Variables

(defn variable [name]
  "Create variable expression."
  {:pre [(keyword? name)]}
  (list ::expr-var name))

(defn variable? [expr]
  "Check if expression represents a variable."
  {:pre [(expression? expr)]}
  (and (= ::expr-var (first expr)) (keyword? (second expr))))

(defn variable-name [expr]
  "Get name of a variable represented by expr."
  {:pre [(variable? expr)]}
  (second expr))

(defn variable-same? [var1, var2]
  "Verify that var1 and var2 represent variables of the same name."
  {:pre [(expression? var1) (expression? var2)]}
  (and
    (variable? var1)
    (variable? var2)
    (= (variable-name var1) (variable-name var2))))

(defn atom? [expr]
  "Check if expr is an atom, i.e. a variable or constant."
  (or
    (variable? expr)
    (constant? expr)))

; Tree traversal support

(defmethod walk-tree ::expr-atom [transformation expr]
  "Stop expansion if atom has been reached."
  {:pre [(atom? expr)]}
  expr)

; Argument accessor

(defmethod args ::expr-atom [expr]
  "Atoms have no arguments."
  `())