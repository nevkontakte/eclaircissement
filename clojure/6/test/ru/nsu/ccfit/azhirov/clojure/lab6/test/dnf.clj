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

(deftest propagate-negation-transform-test
  (is (=
        (disjunction (negation (variable :a)) (negation (variable :b)))
        (propagate-negation-transform
          (negation (conjunction (variable :a) (variable :b))))))
  (is (=
        (conjunction (negation (variable :a)) (negation (variable :b)))
        (propagate-negation-transform
          (negation (disjunction (variable :a) (variable :b))))))
  )

(deftest propagate-negation-test
  (is (=
        (disjunction (variable :a) (negation (variable :b)))
        (propagate-negation
          (negation (conjunction (negation (variable :a)) (variable :b))))))
  (is (=
        (conjunction (variable :a) (negation (variable :b)))
        (propagate-negation-transform
          (negation (disjunction (negation (variable :a)) (variable :b))))))
  )

(deftest disjunct?-test
  (is (disjunct? (constant true?)))
  (is (disjunct? (variable :a)))
  (is (disjunct? (negation (variable :a))))
  (is (disjunct? (conjunction (negation (variable :a)) (variable :b)))) ; (not a) and b
  (is (not (disjunct? (disjunction (negation (variable :a)) (variable :b))))) ; (not a) or b
  (is (not (disjunct? (disjunction (conjunction (negation (variable :a)) (variable :b)) (variable :c))))) ; ((not a) and b) or c
  (is (not (disjunct? (conjunction (disjunction (negation (variable :a)) (variable :b)) (variable :c))))) ; ((not a) and b) or c
  )

(deftest quasi-disjunct?-test
  (is (quasi-disjunct? (constant true?)))
  (is (quasi-disjunct? (variable :a)))
  (is (quasi-disjunct? (negation (variable :a))))
  (is (quasi-disjunct? (conjunction (negation (variable :a)) (variable :b)))) ; (not a) and b
  (is (not (quasi-disjunct? (disjunction (negation (variable :a)) (variable :b))))) ; (not a) or b
  (is (not (quasi-disjunct? (disjunction (conjunction (negation (variable :a)) (variable :b)) (variable :c))))) ; ((not a) and b) or c
  (is (quasi-disjunct? (conjunction (disjunction (negation (variable :a)) (variable :b)) (variable :c)))) ; ((not a) and b) or c
  )

(deftest terms-test
  (is (=
        (list (constant true))
        (terms (constant true))))
  (is (=
        (list (negation (variable :a)))
        (terms (negation (variable :a)))))
  (is (=
        (list (negation (variable :a)) (variable :b))
        (terms (conjunction (negation (variable :a)) (variable :b)))))
  )

(deftest dnf-transform-test
  (is (=
        (disjunction
          (conjunction (variable :a) (variable :b) (variable :c))
          (conjunction (variable :a) (variable :b) (variable :d))
          )
        (dnf-transform (conjunction
                         (variable :a)
                         (variable :b)
                         (disjunction (variable :c) (variable :d))))))
  (is (=
        (disjunction
          (conjunction (variable :a) (variable :b) (disjunction (variable :e) (variable :f)) (variable :c))
          (conjunction (variable :a) (variable :b) (disjunction (variable :e) (variable :f)) (variable :d))
          )
        (dnf-transform (conjunction
                         (variable :a)
                         (variable :b)
                         (disjunction (variable :c) (variable :d))
                         (disjunction (variable :e) (variable :f))))))
  )

(deftest dnf?-test
  (is (dnf? (constant true)))
  (is (dnf? (variable :a)))
  (is (dnf? (negation (variable :a))))
  (is (dnf? (conjunction (variable :a) (variable :b))))
  (is (dnf? (disjunction (variable :a) (variable :b))))
  (is (dnf? (disjunction (variable :a) (conjunction (variable :b) (negation (variable :c))))))
  (is (not (dnf? (conjunction
                   (variable :a)
                   (variable :b)
                   (disjunction (variable :c) (variable :d))))))
  (is (not (dnf? (disjunction
                   (conjunction
                     (variable :a)
                     (variable :b)
                     (disjunction (variable :c) (variable :d)))
                   (variable :e)))))
  )

(deftest dnf-test
  (is (dnf? (dnf (constant true))))
  (is (dnf? (dnf (variable :a))))
  (is (dnf? (dnf (negation (variable :a)))))
  (is (dnf? (dnf (conjunction (variable :a) (variable :b)))))
  (is (dnf? (dnf (disjunction (variable :a) (variable :b)))))
  (is (dnf? (dnf (disjunction (variable :a) (conjunction (variable :b) (negation (variable :c)))))))
  (is (=
        (disjunction (variable :a) (conjunction (variable :b) (negation (variable :c))))
        (dnf (disjunction (variable :a) (conjunction (variable :b) (negation (variable :c)))))))

  (is (=
        (disjunction
          (conjunction (variable :a) (variable :b) (variable :c) (variable :e))
          (conjunction (variable :a) (variable :b) (variable :c) (variable :f))
          (conjunction (variable :a) (variable :b) (variable :d) (variable :e))
          (conjunction (variable :a) (variable :b) (variable :d) (variable :f))
          )
        (dnf (conjunction
               (variable :a)
               (variable :b)
               (disjunction (variable :c) (variable :d))
               (disjunction (variable :e) (variable :f))))))
  )
