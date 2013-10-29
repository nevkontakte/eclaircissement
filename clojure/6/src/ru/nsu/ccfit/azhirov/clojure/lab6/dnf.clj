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

; Phase 2

(defmulti propagate-negation-transform
          "Propagate negation inside operator."
          (fn [expr]
            (if (negation? expr)
              (first (arg expr))
              :default)))

(defmethod propagate-negation-transform :expr-and [expr]
  "Propagate negation inside conjunction: not(a && b) => (not a) || (not b)"
  {:pre [(negation? expr) (conjunction? (arg expr))]}
  (let [expr (arg expr)]
    (apply disjunction (map negation (args expr)))))

(defmethod propagate-negation-transform :expr-or [expr]
  "Propagate negation inside conjunction: not(a || b) => (not a) && (not b)"
  {:pre [(negation? expr) (disjunction? (arg expr))]}
  (let [expr (arg expr)]
    (apply conjunction (map negation (args expr)))))

(defmethod propagate-negation-transform :default [expr]
  "Keep operator as is by default."
  expr)

(defn propagate-negation [expr]
  "Propagate negation signs down to atoms."
  (transform-expression propagate-negation-transform expr))