(ns ru.nsu.ccfit.azhirov.clojure.lab6.follows
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.associatives)
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.not)
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.expr])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.expand])
  )

; Implication

(defn implication? [expr]
  "Check if expr represents implication."
  {:pre [(expression? expr)]}
  (and
    (= ::expr-follows (first expr))
    (expressions? (next expr))
    (= 2 (count (next expr)))))

(defn implication [prerequisite, consequence]
  "Create implication expression."
  (list ::expr-follows prerequisite consequence))

; Expand implication

(defmethod expand-transform ::expr-follows [expr]
  "Expand implication 'a -> b' into '(not a) or b'"
  {:pre [(implication? expr)]}
  (let [prerequisite (expand-transform (first (args expr)))
        consequence (expand-transform (second (args expr)))]
    (disjunction (negation prerequisite) consequence)))

; Polymorphic constructor

(defmethod create ::expr-follows [_, args]
  (implication (first args) (second args)))