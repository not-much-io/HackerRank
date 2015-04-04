(def input-string (read-line))

(defn anagram->palindrome? [s]
  (let [letter-frequencies (frequencies s)
        odd-nr-occurring (count (filter #(odd? (second %))
                                        letter-frequencies))]
    (if (or (= 1 odd-nr-occurring)
            (= 0 odd-nr-occurring))
      true
      false)))

(defn solution-game-of-thrones [s]
  (if (anagram->palindrome? s)
    "YES"
    "NO"))

(println (solution-game-of-thrones input-string))
