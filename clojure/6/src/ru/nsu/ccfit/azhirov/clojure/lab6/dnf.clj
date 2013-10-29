(ns ru.nsu.ccfit.azhirov.clojure.lab6.dnf
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.associatives])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.atoms])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.follows])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.not])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.expr])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.walk-tree])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.expand])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.propagate-negation])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.disjunct])
  )

;; DNF algorithm consists of three phases:
;; 1) Expand complex expressions into composition of "and", "or" and "not".
;; 2) Propagate negation sign to constants or variables.
;; 3) Apply distribution law.

(defn dnf? [expr]
  "Checks if expr is represented in DNF."
  {:pre [(expression? expr)]}
  (every? disjunct? (quasi-disjuncts expr)))

(defn dnf-disjunct-transform [expr]
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

(defn dnf-transform [expr]
  {:doc "Perform a single step of DNF transformation."
   :pre [(expression? expr)]}
  (if (dnf? expr)
    expr
    (recur (apply disjunction (map dnf-disjunct-transform (quasi-disjuncts expr))))))

(defn dnf [expr]
  "Transform expression into DNF."
  ((comp dnf-transform propagate-negation expand) expr))