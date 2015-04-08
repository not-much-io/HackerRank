(ns common-divisors)

; Solution 1

(def input-list 
  (for [line (range (read-string (read-line)))]
    (map read-string (clojure.string/split (read-line) #" "))))

(defn prime? [n]
  (cond (or (= n 2) (= n 3))         true
        (or (< n 2) (= (mod n 2) 0)) false
        (< n 9)                      true
        (= (mod n 3) 0)              false
        :else (let [max             (Math/sqrt n)
                    not-prime-check (fn [val]
                                      (if (or (= (mod n val) 0)
                                              (= (mod n (+ val 2)) 0))
                                        true
                                        false))]
                (loop [val 5]
                  (if (> val max)
                    true
                    (if (not-prime-check val)
                      false
                      (recur (+ val 6))))))))

(def divisors 
  (memoize
   (fn [x]
     (let [sequence (range 1 (inc (/ x 2)))]
       (filter #(= (mod x %) 0) sequence)))))

(use '[clojure.set :only (intersection)])

(defn common-divisors [[m l :as scores]]
  (if (or (prime? m) (prime? l))
    (if (= m l)
      [nil nil]
      [nil])
    (let [divisors-m       (set (divisors m))
          divisors-l       (set (divisors l))
          common-divisors  (intersection divisors-l divisors-m)]
      common-divisors)))

(doall (map #(println (count (common-divisors %))) input-list))

; Solution 2

(defn prime? [n]
  (cond (or (= n 2) (= n 3))         true
        (or (< n 2) (= (mod n 2) 0)) false
        (< n 9)                      true
        (= (mod n 3) 0)              false
        :else (let [max             (Math/sqrt n)
                    not-prime-check (fn [val]
                                      (if (or (= (mod n val) 0)
                                              (= (mod n (+ val 2)) 0))
                                        true
                                        false))]
                (loop [val 5]
                  (if (> val max)
                    true
                    (if (not-prime-check val)
                      false
                      (recur (+ val 6))))))))

(def input-list 
  (for [line (range (read-string (read-line)))]
    (map read-string (clojure.string/split (read-line) #" "))))

(defn gcd 
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn common-divisors [[m l :as scores]]
  (if (or (prime? m) (prime? l))
    (if (= m l)
      [nil nil]
      [nil]))
  (let [gcd-             (gcd m l)
        divisor-for-both #(and (= (mod m %) 0)
                               (= (mod l %) 0))
        common-divisors  (filter divisor-for-both (range 1 (inc gcd-)))]
    common-divisors))

(doall (map #(println (count (common-divisors %))) input-list))

