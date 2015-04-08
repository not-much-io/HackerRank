
(def input-list 
  (doall
   (for [line (range (read-string (read-line)))]
     (read-string (read-line)))))

(defn solution-utopian-tree 
  ([n-cycles]
     (solution-utopian-tree n-cycles 1))
  ([n-cycles current-height]
   (if (zero? n-cycles)
     current-height
     (if (even? current-height)
       (solution-utopian-tree (dec n-cycles) (+ current-height 1))
       (solution-utopian-tree (dec n-cycles) (* current-height 2))))))

(doall (map #(-> % 
                 solution-utopian-tree
                 println) input-list))
