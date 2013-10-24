(ns ru.nsu.ccfit.azhirov.clojure.lab6.expr)

; Common functions

(defn expression? [expr]
  "Check if expr is a valid expression. This function is used mostly as a precondition."
  (and (seq? expr) (not (empty? expr)) (keyword? (first expr))))

(defn expressions? [exprs]
  "Check if all elements if exprs are expressions."
  (and
    (seq exprs)
    (reduce (fn [valid? i] (and valid? (expression? i)))
            true
            exprs)))

; Constants

(defn constant [value]
  "Create constant expression."
  {:pre [(or (true? value) (false? value))]}
  (list ::const value))

(defn constant? [expr]
  "Check if expression represents a constant."
  {:pre [(expression? expr)]}
  (= ::const (first expr)))

(defn constant-value [expr]
  "Get value of constant."
  {:pre [(constant? expr)]}
  (second expr))

; Variables
(defn variable [name]
  "Create variable expression."
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? [expr]
  "Check if expression represents a variable."
  {:pre [(expression? expr)]}
  (and (= ::var (first expr)) (keyword? (second expr))))

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

; Common operations on operators

(defn args [expr]
  "Get arguments of an operatior"
  {:pre [(expression? expr) ; It must be an expression itself,
         (expressions? (next expr))]} ; and all it's arguments must be expressions too.
  (rest expr))

(defn arg [expr]
  "Get single argument of an unary operator"
  {:pre [(expression? expr)
         (expression? (second expr))
         (nil? (second (rest expr)))]}
  (second expr))

; Universal multi-argument operators


(defn multi-op? [kwd expr]
  "Check type of multi-argument operator like conjunction or disjunction."
  {:pre [(keyword? kwd)
         (expression? expr)]}
  (and
    (= kwd (first expr))
    (expressions? (next expr)) ; All args of conjunction must be expressions.
    ))

(defn multi-op [kwd expr & rest]
  "Create multi-argument associative operator like conjunction or disjunction.
  Automatically expands expressions like (a && (b && c) && d) into (a && b && c && d)."
  {:pre [(keyword? kwd)
         (expressions? (cons expr rest))]}
  (if (nil? rest)
    expr
    (let [raw-args (cons expr rest)
          flat-args (reduce (fn [partial-args arg]
                              (if (multi-op? kwd arg)
                                (concat partial-args (args arg))
                                (concat partial-args (list arg))))
                            `()
                            raw-args)
          ]
      (cons kwd flat-args))))

; Conjunction

(def conjunction (partial multi-op ::and))

(def conjunction? (partial multi-op? ::and))

; Disjunction

(def disjunction (partial multi-op ::or))

(def disjunction? (partial multi-op? ::or))

; Negation

(defn negation? [expr]
  "Check if expr represents negation of some expression."
  {:pre [(expression? expr)]}
  (and
    (= ::not (first expr))
    (expression? (second expr))))

(defn negation [expr]
  "Create negation of expression."
  {:pre [(expression? expr)]}
  (if (negation? expr)
    (arg expr)
    (list ::not expr)))

; Implication

(defn implication? [expr]
  "Check if expr represents implication."
  {:pre [(expression? expr)]}
  (and
    (= ::follows (first expr))
    (expressions? (next expr))
    (= 2 (count (next expr)))))

(defn implication [prerequisite, consequence]
  "Create implication expression."
  (list ::follows prerequisite consequence))