(ns captain-prime)

(defn- str->int [c]
  (Integer/parseInt c))

(def ids (for [i (range (str->int (read-line)))]
           (str->int (read-line))))

(defn- prime? [n]
  (cond (or (= n 2) (= n 3))         true
        (or (< n 2) (= (mod n 2) 0)) false
        (< n 9)                      true
        (= (mod n 3) 0)              false
        :else (let [max             (Math/sqrt n)]
                (loop [val 5]
                  (if (> val max) 
                    true
                    (if (or (= (mod n val) 0) (= (mod n (+ val 2)) 0))
                      false
                      (recur (+ val 6))))))))

(defn- contains-zero-digit? [x]
  (.contains (str x) "0"))

(defn- subs-primes? [x f]
  (loop [x (str x)]
    (if (= (count x) 1)
      (prime? (str->int x))
      (let [after-drop (str->int (apply str (f x)))]
        (if (prime? after-drop)
          (recur (str after-drop))
          false)))))

(defn- right-drop-primes? [x]
  (subs-primes? x drop-last))

(defn- left-drop-primes? [x]
  (subs-primes? x rest))

(defn pre-check [id]
  (and (prime? id) (not (contains-zero-digit? id))))

(defn central? [id]
  (and (left-drop-primes? id) (right-drop-primes? id)))

(defn left? [id]
  (left-drop-primes? id))

(defn right? [id]
  (right-drop-primes? id))

(defn faith [id]
  (cond (not (pre-check id)) "DEAD"
        (central?  id) "CENTRAL"
        (left?     id) "LEFT"
        (right?    id) "RIGHT"
        :else "DEAD"))
        
(doall (map #(println (faith %)) ids))
