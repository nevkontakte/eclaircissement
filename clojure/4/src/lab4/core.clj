(ns lab4.core)

(defn wordlist
  "Generate worl list of specified length for an alphabet"
  [n alpha]
  (defn generate_words [lst, word]
    (let
      [alpha_filtered (filter (fn [symbol] (not (= symbol (str (last word))))) alpha)
       new_words (map (fn [x] (str word x)) alpha_filtered)]

      (concat lst new_words)
      )
    )
  (reduce
    (fn [full_list _]
      (reduce generate_words `() full_list)
      )
    `("")
    (range 0 n))
  )