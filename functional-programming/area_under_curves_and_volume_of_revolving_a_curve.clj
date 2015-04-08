(def as (map read-string (clojure.string/split (read-line) #" ")))
(def bs (map read-string (clojure.string/split (read-line) #" ")))
(def lims (let [input (clojure.string/split (read-line) #" ")]
            {:upper (read-string (second input))
             :lower (read-string (first input))}))

(defn alg-expression 
  ([x as bs]
   (alg-expression x as bs []))
  ([x as bs results]
   (if (empty? as)
     (apply +
            results)
     (let [to-pwr-var (Math/pow x (first bs))
           times-var (first as)
           new-results (conj results (* times-var
                                        to-pwr-var))]
       (recur x (rest as) (rest bs) new-results)))))

(def n-points (range (:lower lims) 
                     (:upper lims)
                     0.001))

(def n-heights  
  (for [i n-points]
    (alg-expression i as bs)))

(def widths (/ (- (:upper lims)
                  (:lower lims))
               (count n-points)))

(defn circle-area [r]
  (* Math/PI (Math/pow r 2)))

(defn disk-volume [h]
  (* widths (circle-area h)))

(println
 (apply +
        (map #(* %1 widths) 
             n-heights)))

(println
 (apply +
        (map disk-volume
             n-heights)))
