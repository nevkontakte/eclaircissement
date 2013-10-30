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

(defn associative-operator [op-name expr & rest]
  "Create multi-argument associative operator like conjunction or disjunction.
  Automatically expands expressions like (a && (b && c) && d) into (a && b && c && d)."
  {:pre [(keyword? op-name)
         (expressions? (cons expr rest))]}
  (if (nil? rest)
    expr
    (let [raw-args (cons expr rest)
          flat-args (reduce (fn [partial-args arg]
                              (if (associative-operator? op-name arg)
                                (concat partial-args (args arg))
                                (concat partial-args (list arg))))
                            `()
                            raw-args)
          ]
      (cons op-name flat-args))))

; Conjunction

(def conjunction (partial associative-operator ::expr-and))

(def conjunction? (partial associative-operator? ::expr-and))

; Disjunction

(def disjunction (partial associative-operator ::expr-or))

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
