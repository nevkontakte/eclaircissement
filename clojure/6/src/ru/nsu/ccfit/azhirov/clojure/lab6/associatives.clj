(ns ru.nsu.ccfit.azhirov.clojure.lab6.associatives
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.expr])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.propagate-negation])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.not])
  )

; Universal multi-argument operators

(defn associative-operator? [op-name expr]
  "Check type of multi-argument operator like conjunction or disjunction."
  {:pre [(keyword? op-name)
         (expression? expr)]}
  (and
    (= op-name (first expr))
    (expressions? (next expr)) ; All args of conjunction must be expressions.
    ))

(defn flattern-args [op-name exprs]
  "Apply associative rule to operator args."
  (reduce (fn [processed current-arg]
            (if (associative-operator? op-name current-arg)
              (concat processed (args current-arg))
              (concat processed (list current-arg))))
          `()
          exprs))


(defmethod create ::expr-assoc [op-name op-args]
  "Create multi-argument associative operator like conjunction or disjunction.
  Automatically expands expressions like (a && (b && c) && d) into (a && b && c && d)."
  {:pre [(keyword? op-name)
         (expressions? op-args)]}
  (let [flat-args (flattern-args op-name op-args)
        deduplicated-args (distinct flat-args)]
    (if (seq (next deduplicated-args))
      (cons op-name deduplicated-args)
      (first deduplicated-args))))

(derive ::expr-and ::expr-assoc)
(derive ::expr-or ::expr-assoc)

; Conjunction

(defn conjunction [ & args] (create ::expr-and args))

(def conjunction? (partial associative-operator? ::expr-and))

; Disjunction

(defn disjunction [ & args] (create ::expr-or args))

(def disjunction? (partial associative-operator? ::expr-or))

; Propagate negation

(defmethod propagate-negation-transform ::expr-and [expr]
  "Propagate negation inside conjunction: not(a && b) => (not a) || (not b)"
  {:pre [(negation? expr) (conjunction? (arg expr))]}
  (let [expr (arg expr)]
    (apply disjunction (map negation (args expr)))))

(defmethod propagate-negation-transform ::expr-or [expr]
  "Propagate negation inside conjunction: not(a || b) => (not a) && (not b)"
  {:pre [(negation? expr) (disjunction? (arg expr))]}
  (let [expr (arg expr)]
    (apply conjunction (map negation (args expr)))))
