
(def input-list 
  (doall
   (for [line (range (read-string (read-line)))]
     (read-line))))

(defn rotate-string 
  ([s] (if (= (count s) 1)
         [s]
         (rotate-string s [] (count s))))
  ([s rotations rotations-to-go] 
   (if (zero? rotations-to-go)
     rotations
     (let [new-rotation (apply str (concat (rest s)
                                           (vector (first s))))
           new-rotations (conj rotations new-rotation)]
       (recur new-rotation new-rotations (dec rotations-to-go))))))

(doall (map #(do 
               (apply print %)
               (println))
        (map rotate-string input-list)))
