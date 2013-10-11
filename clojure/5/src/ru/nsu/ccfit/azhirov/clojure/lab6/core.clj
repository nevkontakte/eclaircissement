(ns ru.nsu.ccfit.azhirov.clojure.lab6.core)


(defn trapeze [f x1 x2]
  (*
    (/
      (+ (f x1) (f x2))
      2)
    (Math/abs (- x2 x1))))

(defn integrate [f, delta]
  (let [f_memo (memoize f)
        generator (fn [plus_or_minus [x1, integral]]
                    (let [x2 (plus_or_minus x1 delta)]
                      [x2 (+ integral (trapeze f_memo x1 x2))]))
        seq_pos (map first (iterate (partial generator +) [0, 0]))
        seq_neg (map first (iterate (partial generator -) [0, 0]))]
    (fn [x]
      (let [seq (if (pos? x) seq_pos seq_neg)
            sign (if (neg? x) -1 1)
            x (Math/abs x)
            steps (quot x delta)
            tail (mod x delta)]
        (+
          (nth seq steps)
          (trapeze f_memo
            (* sign (- x tail))
            (* sign x)))
        ))))