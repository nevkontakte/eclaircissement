(ns ru.nsu.ccfit.azhirov.clojure.lab6.dnf
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.expr)
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.expr :as expr]))

;; DNF algorithm consists of three phases:
;; 1) Expand complex expressions into composition of "and", "or" and "not".
;; 2) Propagate negation sign to constants or variables.
;; 3) Apply distribution law.

; Expression tree transformation routines

(defmulti walk-tree
          "Tree traversal routine for transform-expression."
          (fn [transform expr] (first expr)))


(defn transform-expression [transform expr]
  "Walk expression tree and apply a transformation to it."
  (let [expr (transform expr)]
    (walk-tree transform expr)))


(defmethod walk-tree :expr-const [transformation expr]
  "Stop expansion if const has been reached."
  {:pre [(constant? expr)]}
  expr)

(defmethod walk-tree :expr-var [transformation expr]
  "Stop expansion if variable has been reached."
  {:pre [(variable? expr)]}
  expr)

(defmethod walk-tree :default [transformation expr]
  "By default do not alter an operator, but expand it's operands."
  {:pre [(expression? expr)]}
  (cons (first expr) (map (partial transform-expression transformation) (args expr))))

; Phase 1

(defmulti expand-transform
          "Expand operator in terms of 'and', 'or' and 'not'."
          first)

(defmethod expand-transform :expr-follows [expr]
  "Expand implication 'a -> b' into '(not a) or b'"
  {:pre [(implication? expr)]}
  (let [prerequisite (expand-transform (first (args expr)))
        consequence (expand-transform (second (args expr)))]
    (disjunction (negation prerequisite) consequence)))

(defmethod expand-transform :default [expr]
  "By default do not alter an operator, but expand it's operands."
  {:pre [(expression? expr)]}
  expr)

(defn expand [expr]
  "Expand expression tree in terms of 'and', 'or' and 'not'."
  (transform-expression expand-transform expr))

