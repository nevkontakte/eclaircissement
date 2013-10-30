(ns ru.nsu.ccfit.azhirov.clojure.lab6.test.assign
  (:use [clojure.test])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.expr])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.atoms])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.associatives])
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.assign])
  )

(deftest assign-test
  (is (=
        (constant true)
        (assign {:a true} (variable :a))))
  (is (=
        (variable :b)
        (assign {:a true} (variable :b))))
  (is (=
        (disjunction (constant true) (variable :b))
        (assign {:a true} (disjunction (variable :a) (variable :b)))))
  )