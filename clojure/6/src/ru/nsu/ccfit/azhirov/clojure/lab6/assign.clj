(ns ru.nsu.ccfit.azhirov.clojure.lab6.assign
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.expr)
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.atoms)
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.walk-tree)
  )

(defn assign [value-map expr]
  "Assign values to variables in expression. value-map must be a map, where keys are variable names and values — corresponding var values."
  (transform-expression (fn [node]
                          (if (and (variable? node) (contains? value-map (variable-name node)))
                            (constant (get value-map (variable-name node) node))
                            node))
                        expr))
