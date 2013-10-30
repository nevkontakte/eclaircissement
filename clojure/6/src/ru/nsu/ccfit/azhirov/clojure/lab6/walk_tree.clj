(ns ru.nsu.ccfit.azhirov.clojure.lab6.walk-tree
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.expr]))

; Expression tree transformation routines

(defmulti walk-tree
          "Tree traversal routine for transform-expression."
          (fn [transform expr] (first expr)))


(defn transform-expression [transform expr]
  "Walk expression tree and apply a transformation to it."
  (let [expr (transform expr)]
    (walk-tree transform expr)))


(defmethod walk-tree :default [transformation expr]
  "By default do not alter an operator, but expand it's operands."
  {:pre [(expression? expr)]}
  (create (first expr) (map (partial transform-expression transformation) (args expr))))

