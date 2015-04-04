;Initial solution

(def input-list 
  (doall
   (for [line (range (read-string (read-line)))]
     (read-string (read-line)))))

(defn factorial [n]
  (reduce * (range 1 (inc n))))

(defn series-expansion [x]
  (let [pre-series  [1 x]
        series-calc (fn [i]
                      (let [rais (Math/pow x i)
                            fact (factorial i)]
                        (/ rais fact)))
        post-series (map series-calc (range 2 10))
        series      (concat pre-series 
                            post-series)]
    (apply + series)))

(doall
 (map println 
      (map series-expansion 
           input-list)))
