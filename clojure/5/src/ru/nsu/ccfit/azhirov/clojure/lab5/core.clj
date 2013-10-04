(ns ru.nsu.ccfit.azhirov.clojure.lab5.core)


(defn trapeze [f x1 x2]
  (*
    (/
      (+ (f x1) (f x2))
      2)
    (Math/abs (- x2 x1))))

(defn int_step [myself, f, step, delta]
  (if (<= step 0)
    0
    (let [x2 (* step delta)
          x1 (- x2 delta)]
      (+
        (myself myself f (dec step) delta)
        (trapeze f x1 x2)
        ))
    )
  )

(defn integrate [f, delta]
  (let [f_memo (memoize f)
        int_step_memo (memoize int_step)]
    (fn [x]
      (let [sign (if (neg? x) -1 1)
            delta (* delta sign)
            steps (quot x delta)
            tail (mod x delta)]
        (*
          sign
          (+
            (int_step_memo int_step_memo f_memo steps delta)
            (trapeze f_memo (- x tail) x)))
        ))))