(def input-list 
  (doall
   (for [line (range (read-string (read-line)))]
     (read-string (read-line)))))


(defn find-minimal-del
  ([letter-seq]
   (find-minimal-del (rest letter-seq) (first letter-seq) 0))
  ([letter-seq last del-count]
   (if (empty? letter-seq)
     del-count
     (let [l1 (first letter-seq)
           del (= l1 last)
           new-last (if del
                      last
                      l1)
           del-count (if (= last l1)
                       (inc del-count)
                       del-count)]
       (find-minimal-del (rest letter-seq) new-last del-count)))))
       
(doall (map #(-> % 
                str 
                find-minimal-del
                println) input-list))
