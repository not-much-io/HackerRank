(def input-list 
   (for [line (range (read-string (read-line)))]
     (read-string (read-line))))

(defn pentagonal-nr [n]
  (/ (- (* 3 (Math/pow n 2)) n) 2))
  
(doall (map #(println (biginteger (pentagonal-nr %))) input-list))
