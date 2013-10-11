(ns ru.nsu.ccfit.azhirov.clojure.lab6.test.core
  (:use [ru.nsu.ccfit.azhirov.clojure.lab6.core])
  (:use [clojure.test]))

(defn float-eq [expected actual eps]
  (and
    (> actual (- expected eps))
    (< actual (+ expected eps))
    ))

(letfn [(f [x] x)]
  (deftest trapeze-test
    (is (= 1/2 (trapeze f 0 1)))
    (is (= (float 3/8) (trapeze f 0.5 1)))
    (is (= (float 3/8) (trapeze f 1 0.5)))
    ))

(let [eps 0.01
      f (integrate (fn [x] 1) 0.1)]
  (deftest const-integration
    (is (function? f))
    (is (float-eq 0 (f 0) eps))
    (is (float-eq 50 (f 50) eps))
    (is (float-eq 51 (f 51) eps))
    (is (float-eq -50 (f -50) eps))
    )
  )

(let [eps 0.01
      f (integrate (fn [x] 1) 0.1)]
  (deftest perf
    (is (float-eq
          125
          (time (f 125))
          eps))
    (is (float-eq 126 (time (f 126)) eps))
    )
  )

(let [eps 0.01
      f (integrate (fn [x] (+ x (Math/sin x))) 0.1)]
  (deftest complex-integration
    (is (float-eq 0 (f 0) eps))
    (is (= (f 1) (- (f -1))))
    )
  )
