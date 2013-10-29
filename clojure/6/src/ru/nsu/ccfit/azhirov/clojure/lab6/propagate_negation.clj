(ns ru.nsu.ccfit.azhirov.clojure.lab6.propagate-negation
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.not)
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.expr)
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.associatives)
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.walk-tree)
  )

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

