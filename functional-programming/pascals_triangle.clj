(def k (read-string (read-line)))

(defn factorial [n]
  (loop [cnt n acc 1]
    (if (zero? cnt)
      acc
      (recur (dec cnt) (* acc cnt)))))

(defn value-at-pos [row column]
  (/ (factorial row)
     (* (factorial column)
        (factorial (- row column)))))

(defn pascals-triangle 
  ([k] (pascals-triangle k []))
  ([k acc]
   (if (> k 0)
     (let [new-acc (conj acc (map #(value-at-pos (dec k) %) (range k)))]
       (recur (dec k) new-acc))
     (reverse acc))))

(doall (map #(do (doall (for [el %]
                          (print (str el " "))))
                 (println)) 
          (pascals-triangle k)))
