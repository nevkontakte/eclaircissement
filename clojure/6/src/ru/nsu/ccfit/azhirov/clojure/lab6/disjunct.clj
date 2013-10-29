(ns ru.nsu.ccfit.azhirov.clojure.lab6.disjunct
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.not)
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.atoms)
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.expr)
  (:use ru.nsu.ccfit.azhirov.clojure.lab6.associatives)
  )

(defn term? [expr]
  "Check if expr is a term, i.e. an atom or atom negation."
  (or
    (atom? expr)
    (and
      (negation? expr)
      (atom? (arg expr)))))

(defn disjunct? [expr]
  "Checks if expr forms a valid disjunct. It must be a term or conjunction of terms."
  {:pre [(expression? expr)]}
  (or
    (term? expr)
    (and
      (conjunction? expr)
      (every? term? (args expr)))))


(defn quasi-disjunct? [expr]
  "Check if expr froms a disjunct but also accept a conjucntion of an arbitrary expressions, not only terms."
  {:pre [(expression? expr)]}
  (or
    (conjunction? expr)
    (disjunct? expr)))


(defn terms [expr]
  "List all terms of a quasi-disjunct expr."
  {:pre [(quasi-disjunct? expr)]}
  (if (term? expr)
    (list expr)
    (args expr)))


(defn quasi-disjuncts [expr]
  {:pre [(expression? expr)]
   :post [(every? quasi-disjunct? %)]}
  "List all top-level quasi-disjuncts of an expression expr. Expr must be expanded and negation in expr must be propagated."
  (if (quasi-disjunct? expr)
    (list expr)
    (args expr)))
