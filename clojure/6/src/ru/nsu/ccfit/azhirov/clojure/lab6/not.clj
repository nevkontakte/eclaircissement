(ns ru.nsu.ccfit.azhirov.clojure.lab6.not
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.expr]))

; Negation

(defn negation? [expr]
  "Check if expr represents negation of some expression."
  {:pre [(expression? expr)]}
  (and
    (= :expr-not (first expr))
    (expression? (second expr))))

(defn negation [expr]
  "Create negation of expression."
  {:pre [(expression? expr)]}
  (if (negation? expr)
    (arg expr)
    (list :expr-not expr)))


