(ns ru.nsu.ccfit.azhirov.clojure.lab6.follows
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.expr]))

; Implication

(defn implication? [expr]
  "Check if expr represents implication."
  {:pre [(expression? expr)]}
  (and
    (= :expr-follows (first expr))
    (expressions? (next expr))
    (= 2 (count (next expr)))))

(defn implication [prerequisite, consequence]
  "Create implication expression."
  (list :expr-follows prerequisite consequence))

