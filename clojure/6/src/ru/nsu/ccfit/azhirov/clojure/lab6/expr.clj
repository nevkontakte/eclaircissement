(ns ru.nsu.ccfit.azhirov.clojure.lab6.expr)

; Basic expression definitions

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

; Helper functions for accessing expression args

(defmulti args "Get list of arguments of an operator." first)

(defmethod args :default [expr]
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

