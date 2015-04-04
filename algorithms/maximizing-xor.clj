
(defn max-xor [l r]
  (let [bigger (apply max [l r])
        smaller (apply min [l r])
        interval (range smaller (inc bigger))]
    (apply max
           (flatten
            (for [i interval]
              (for [j interval]
                (bit-xor i j)))))))

(let [_l (Integer/parseInt (read-line))
      _r (Integer/parseInt (read-line))]
  (println (max-xor _l _r)))
