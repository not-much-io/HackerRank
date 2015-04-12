(ns missing-numbers)

(defn- str->int [c]
  (Integer/parseInt c))

(defn- get-input []
  (let [_ (read-line)
        inp (clojure.string/split (read-line) #" ")]
    (map str->int inp)))

(defn solve [l1 l2]
  (loop [uniqs (set l2)
         freq1 (frequencies l1)
         freq2 (frequencies l2)
         acc   []]
    (if-let [el (first uniqs)]
      (let [new-acc (if (= (get freq1 el) (get freq2 el))
                      acc
                      (conj acc el))]
        (recur (rest uniqs) freq1 freq2 new-acc)))))

(let [l1 (get-input)
      l2 (get-input)
      uniques (set l2)
      freq-1  (frequencies l1)
      freq-2  (frequencies l2)]
  (map println
       (loop [unqs uniques
              acc  []]
         )))


















