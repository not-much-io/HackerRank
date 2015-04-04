(defn input []
  (read-string (read-line)))

(def seq-len (input))
(def sub-seq-len (input))
(def input-seq (for [i (range seq-len)]
                 (input)))

(defn min-unfairness 
  ([sequence]
   (min-unfairness sequence 
                   0
                   (Integer/MAX_VALUE)))
  ([sequence indx min-unf]
   (if (> (+ sub-seq-len indx) seq-len)  
     min-unf
     (let [new-unf (- (nth sequence
                           (dec (+ indx
                                   sub-seq-len)))
                      (nth sequence
                           indx))
           new-min-unf (if (< new-unf min-unf)
                         new-unf
                         min-unf)
           new-indx (inc indx)]
       (recur sequence new-indx new-min-unf)))))

(defn solution-max-min [sequence]
  (let [sorted-sequence (vec (sort sequence))
        min-unf (min-unfairness sorted-sequence)]
    min-unf))

(println (solution-max-min input-seq))
