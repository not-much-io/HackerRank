(ns lists-and-gcd)

(defn- str->int [c]
  (Integer/parseInt c))

(defn- in? [seq el]
  (some #(= el %) seq))

(use 'clojure.set)

(defn- bases-in-all-factors [factors]
  (let [bases (->> factors 
                   (map #(partition 2 %))
                   (map #(map first %)))]
    (apply intersection (map set bases))))

(defn- lowest-power-factors [bases factors]
  (loop [bases (sort bases)
         pairs (partition 2 (flatten factors))
         acc   []]
    (if (empty? bases)
      acc
      (let [curr-base        (first bases)
            relevant-factors (filter #(= (first %) curr-base) pairs)
            powers           (map second relevant-factors)
            lowest-power     (apply min powers)
            new-acc          (conj acc [curr-base lowest-power])]
        (recur (rest bases) pairs new-acc)))))

(def factors (for [i (range (str->int (read-line)))]
               (map str->int (clojure.string/split (read-line) #" "))))

(let [relevant-bases (bases-in-all-factors factors)
      answer         (lowest-power-factors relevant-bases factors)]
  (doall (map #(print % "") (flatten answer))))
