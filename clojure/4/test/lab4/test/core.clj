(ns lab4.test.core
  (:use [lab4.core])
  (:use [clojure.test]))

(deftest wordlist_abc
  (is (= `("") (wordlist 0 `("a", "b", "c"))))
  (is (= `("a", "b", "c") (wordlist 1 `("a", "b", "c"))))
  (is (= `("ab", "ac", "ba", "bc", "ca", "cb") (wordlist 2 `("a", "b", "c"))))
  )

(deftest wordlist_a
  (is (= `("") (wordlist 0 `("a"))))
  (is (= `("a") (wordlist 1 `("a"))))
  (is (= `() (wordlist 2 `("a"))))
  )

(deftest wordlist_empty
  (is (= `("") (wordlist 0 `())))
  (is (= `() (wordlist 1 `())))
  (is (= `() (wordlist 2 `())))
  )