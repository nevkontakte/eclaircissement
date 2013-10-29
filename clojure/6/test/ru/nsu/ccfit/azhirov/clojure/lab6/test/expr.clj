(ns ru.nsu.ccfit.azhirov.clojure.lab6.test.expr
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.expr])
  (:use [clojure.test]))

; Constants

(deftest constant-test
  (is (expression? (constant true)))
  (is (expression? (constant false)))
  )

(deftest constant?-test
  (is (constant? (constant true)))
  (is (not (constant? `(w t f))))
  (is (not (constant? `())))
  (is (not (constant? "")))
  )

(deftest constant-value-test
  (is (= true (constant-value (constant true))))
  (is (= false (constant-value (constant false))))
  )

; Variables

(deftest variable-test
  (is (expression? (variable :a)))
  (is (not (constant? (variable :a))))
  )

(deftest variable?-test
  (is (variable? (variable :a)))
  (is (variable? (variable :b)))
  (is (not (variable? "")))
  (is (not (variable? `())))
  (is (not (variable? (constant true))))
  )

(deftest variable-name?-test
  (is (= :a (variable-name (variable :a))))
  (is (not (= :a (variable-name (variable :b)))))
  )

(deftest variable-same?-test
  (is (variable-same? (variable :a) (variable :a)))
  (is (not (variable-same? (variable :a) (variable :b))))
  (is (not (variable-same? (variable :a) (constant true))))
  (is (not (variable-same? (constant true) (variable :a))))
  )

; Operators

(deftest args-test
  (is (=
        (list (variable :a) (variable :b))
        (args (conjunction (variable :a) (variable :b)))))
  (is (=
        (list (variable :a) (variable :b))
        (args (disjunction (variable :a) (variable :b)))))
  (is (=
        (list (variable :a) (variable :b))
        (args (implication (variable :a) (variable :b)))))
  (is (=
        (list (variable :a))
        (args (negation (variable :a)))))
  )

(deftest arg-test
  (is (=
        (variable :a)
        (arg (negation (variable :a)))))
  )

(deftest conjunction-test
  (is (expression? (conjunction (variable :a) (variable :b))))
  (is (not (variable? (conjunction (variable :a) (variable :b)))))
  (is (not (variable? (conjunction (variable :a) (variable :b) (variable :c)))))
  (is (variable? (conjunction (variable :a))))
  )

(deftest conjunction?-test
  (is (not (conjunction? (variable :a))))
  (is (not (conjunction? (conjunction (variable :a)))))
  (is (conjunction? (conjunction (variable :a) (variable :b))))
  (is (conjunction? (conjunction (variable :a) (variable :b) (variable :c))))
  )

(deftest disjunction-test
  (is (expression? (disjunction (variable :a) (variable :b))))
  (is (not (variable? (disjunction (variable :a) (variable :b)))))
  (is (not (variable? (disjunction (variable :a) (variable :b) (variable :c)))))
  (is (variable? (disjunction (variable :a))))
  (is (not (=
             (disjunction (variable :a) (variable :b))
             (conjunction (variable :a) (variable :b)))))
  )

(deftest disjunction?-test
  (is (not (disjunction? (variable :a))))
  (is (not (disjunction? (disjunction (variable :a)))))
  (is (not (disjunction? (conjunction (variable :a) (variable :b)))))
  (is (disjunction? (disjunction (variable :a) (variable :b))))
  (is (disjunction? (disjunction (variable :a) (variable :b) (variable :c))))
  )

(deftest multi-op-flat-test
  (is (=
        (disjunction (variable :a) (disjunction (variable :b) (variable :c)))
        (disjunction (variable :a) (variable :b) (variable :c))
        ))
  (is (=
        (disjunction (variable :a) (disjunction (variable :b) (disjunction (variable :c) (variable :d))))
        (disjunction (variable :a) (variable :b) (variable :c) (variable :d))
        ))
  )

(deftest negation-test
  (is (expression? (negation (variable :a))))
  (is (not (conjunction? (negation (variable :a)))))
  (is (=
        (variable :a)
        (negation (negation (variable :a)))))
  )

(deftest negation?-test
  (is (negation? (negation (variable :a))))
  (is (not (negation? (conjunction (variable :a) (variable :b)))))
  (is (not (negation? (negation (negation (variable :a))))))
  )

(deftest implication-test
  (is (expression? (implication (variable :a) (variable :b))))
  (is (= (variable :a)
         (first (args (implication (variable :a) (variable :b))))))
  (is (= (variable :b)
         (second (args (implication (variable :a) (variable :b))))))
  )

(deftest implication?-test
  (is (implication? (implication (variable :a) (variable :b))))
  (is (not (implication? (conjunction (variable :a) (variable :b)))))
  )

; Atoms and terms

(deftest atom?-test
  (is (atom? (constant true)))
  (is (atom? (constant false)))
  (is (atom? (variable :a)))
  (is (not (atom? (negation (constant true)))))
  (is (not (atom? (negation (variable :a)))))
  )

(deftest term?-test
  (is (term? (constant true)))
  (is (term? (constant false)))
  (is (term? (variable :a)))
  (is (term? (negation (constant true))))
  (is (term? (negation (variable :a))))
  (is (not (term? (conjunction (variable :a) (variable :b)))))
  (is (not (term? (disjunction (variable :a) (variable :b)))))
  )