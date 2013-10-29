(ns ru.nsu.ccfit.azhirov.clojure.lab6.expand
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.follows)
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.associatives)
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.not)
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.expr)
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.walk-tree)
  )

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
