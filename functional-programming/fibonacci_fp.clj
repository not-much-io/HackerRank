(ns fibonacci-fp)

(def nr-tcs (Integer/parseInt (read-line)))
(def tcs (for [i (range nr-tcs)]
           (Integer/parseInt (read-line))))

(def fib-sequence (map first 
                       (iterate (fn [[a b]] [b (+' a b)]) 
                                [0 1])))
(def to-mod-by (int (+ (Math/pow 10 8) 7)))

(defn fib [n] 
  (nth fib-sequence n))

(let [form-ans (fn [x]
                 (.intValue (mod x to-mod-by)))
      answers  (map fib tcs)]
  (doall 
   (for [ans answers]
     (println (form-ans ans)))))
