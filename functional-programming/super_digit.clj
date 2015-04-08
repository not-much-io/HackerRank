(ns super-digit)

(defn- char||str->int [c]
  (Integer/parseInt (str c)))

(defn- digits-sum [val]
  (apply + (map char||str->int val)))

(defn super-digit [val]
  (if (= 1 (count val))
    val
    (let [sum (digits-sum val)]
      (recur (str sum)))))

(let [[n k]       (clojure.string/split (read-line) #" ")
      init-sum    (digits-sum n)
      initial-val (str (* (char||str->int k) init-sum))]
  (println (super-digit initial-val)))
