(ns jumping_bunnies)

(defn- str->int [c]
  (Integer/parseInt c))

(def jump-distances (do
                      (read-line) ;ignore
                      (map str->int (clojure.string/split (read-line) #" "))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (*' a b) (gcd a b)))

(println (str (reduce lcm jump-distances)))
