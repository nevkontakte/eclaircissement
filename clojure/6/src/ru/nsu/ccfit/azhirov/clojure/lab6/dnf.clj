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

; Phase 3

(defn disjunct? [expr]
  "Checks if expr forms a valid disjunct. It must be a term or conjunction of terms."
  {:pre [(expression? expr)]}
  (or
    (term? expr)
    (and
      (conjunction? expr)
      (every? term? (args expr)))))


(defn quasi-disjunct? [expr]
  "Check if expr froms a disjunct but also accept a conjucntion of an arbitrary expressions, not only terms."
  {:pre [(expression? expr)]}
  (or
    (conjunction? expr)
    (disjunct? expr)))

(defn quasi-disjuncts [expr]
  {:pre [(expression? expr)]
   :post [(every? quasi-disjunct? %)]}
  "List all top-level quasi-disjuncts of an expression expr. Expr must be expanded and negation in expr must be propagated."
  (if (quasi-disjunct? expr)
    (list expr)
    (args expr)))

(defn terms [expr]
  "List all terms of a quasi-disjunct expr."
  {:pre [(quasi-disjunct? expr)]}
  (if (term? expr)
    (list expr)
    (args expr)))

(defn dnf? [expr]
  "Checks if expr is represented in DNF."
  {:pre [(expression? expr)]}
  (every? disjunct? (quasi-disjuncts expr)))

(defn dnf-transform [expr]
  "Perform a single step of DNF transformation."
  {:pre [(quasi-disjunct? expr)]}
  (if (disjunct? expr)
    expr
    (let ; If quasi-disjunct is not a disjunct, then if be a conjunction with at least one operand, which is a disjunction.
        [all-terms (terms expr)
         pseudo-term (first (filter (comp not term?) all-terms)) ; Find first non-term operand, it must be a disjunction
         other-terms (remove #{pseudo-term} all-terms) ; Produce list of other operands
         new-term-args (map (fn [t] (apply conjunction (concat other-terms (list t))))
                         (args pseudo-term)) ; Apply distribution law: [a and b and (c or d)] => [(a and b and c) or (a and b and d)]
         new-term (apply disjunction new-term-args)
         ]
      new-term)
    ))

(defn dnf [expr]
  {:doc "Perform a single step of DNF transformation."
   :pre [(expression? expr)]}
  (if (dnf? expr)
    expr
    (recur (apply disjunction (map dnf-transform (quasi-disjuncts expr))))))
