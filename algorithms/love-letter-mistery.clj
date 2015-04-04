(def input-list 
  (doall
   (for [line (range (read-string (read-line)))]
     (read-string (read-line)))))

(defn decs-to-make-match [l1 l2]
  (Math/abs (- (int l1) (int l2))))

(defn min-decs-for-palindrome 
  ([word]
   (min-decs-for-palindrome word 0))
  ([remaining-word nr-decs]
   (let [len (count remaining-word)]
     (if (or (= len 0)
             (= len 1))
       nr-decs
       (let [start-letter (first remaining-word)
             end-letter (last remaining-word)
             decs (+ (decs-to-make-match start-letter end-letter)
                     nr-decs)]
         (recur (subs remaining-word
                      1
                      (- len 1)) decs))))))

(doall (map #(-> %
                 str
                 min-decs-for-palindrome
                 println) input-list))


