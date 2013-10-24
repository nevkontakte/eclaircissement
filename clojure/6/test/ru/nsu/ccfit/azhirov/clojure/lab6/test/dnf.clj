(ns ru.nsu.ccfit.azhirov.clojure.lab6.test.dnf
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.dnf])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.expr :as expr])
  (:use [clojure.test]))

(defn reverse-transform [expr]
  (cons (first expr) (reverse (args expr))))

(deftest transform-expression-test
  (is (= (variable :a)
         (transform-expression identity (variable :a))))
  (is (= (disjunction
           (negation (conjunction
                       (variable :a)
                       (variable :b)))
           (disjunction
             (variable :c)
             (conjunction (variable :d) (variable :e))))
         (transform-expression
           reverse-transform
           (disjunction
             (disjunction
               (conjunction (variable :e) (variable :d))
               (variable :c))
             (negation (conjunction
                         (variable :b)
                         (variable :a)))))))
  )

(deftest expand-invariants
  (is (= (constant true)
         (expand (constant true))))
  (is (= (variable :a)
         (expand (variable :a))))
  (is (= (negation (variable :a))
         (expand (negation (variable :a)))))
  (is (= (conjunction (variable :a) (variable :b))
         (expand (conjunction (variable :a) (variable :b)))))
  (is (= (disjunction (variable :a) (variable :b))
         (expand (disjunction (variable :a) (variable :b)))))
  )

(deftest expand-recursion
  (is (=
        (negation (disjunction (negation (variable :a)) (variable :b)))
        (expand (negation (implication (variable :a) (variable :b))))
        ))
  (is (=
        (conjunction
          (disjunction (negation (variable :a)) (variable :b))
          (disjunction (negation (variable :c)) (variable :d)))
        (expand (conjunction
                  (implication (variable :a) (variable :b))
                  (implication (variable :c) (variable :d))))
        ))
  )

(deftest expand-implication
  (is (= (disjunction (negation (variable :a)) (variable :b))
         (expand (implication (variable :a) (variable :b))))))