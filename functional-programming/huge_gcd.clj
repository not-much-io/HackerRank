(defn gcd 
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn get-input []
  (do
    (read-line) ;Ignore nr of ints
    (apply *' (map read-string (clojure.string/split (read-line) #" ")))))

(def A (get-input))
(def B (get-input))

(println (biginteger (mod (gcd A B) 1000000007)))
