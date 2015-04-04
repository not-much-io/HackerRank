
(def n (read-string (read-line)))
(def rows 32)
(def columns 63)

(defn concat-triangles [t1 t2]
  (map concat
       t1
       (repeat (count t1) "_")
       t2))

(defn add-buffer [ones width]
  (if (or (= (count ones) width)
          (= (count ones) (inc width)))
    ones
    (recur (concat "_" ones "_") width)))

(defn create-triangle [scale]
  (let [height scale
        width (int (- (* height 2) 1))
        ones (map #(repeat % "1")
                  (take height
                        (range 1 (Integer/MAX_VALUE) 2)))]
    (map #(add-buffer % width) ones)))

(defn sierpinski-triangle 
  ([n]
   (if (zero? n)
     (create-triangle rows)
     (let [nr-of-lvls (Math/pow 2 n)
           triangle-heights (int (/ rows nr-of-lvls))
           top-triangle (create-triangle triangle-heights)
           formatted-top-triangle (map #(add-buffer % columns)
                                       top-triangle)]
       (sierpinski-triangle n top-triangle formatted-top-triangle))))
  ([n last-triangle formatted-last-triangle]
   (if (zero? n)
     formatted-last-triangle
     (let [current-level (concat-triangles last-triangle 
                                           last-triangle)
           current-triangle (concat (map #(add-buffer % (count (first current-level)))
                                         last-triangle)
                                    current-level)
           current-triangle-formatted (concat (map #(add-buffer % columns) 
                                                   last-triangle)
                                              (map #(add-buffer % columns)
                                                   current-level))]
       (recur (dec n) current-triangle current-triangle-formatted)))))


(doall (map println
            (map #(apply str %) (sierpinski-triangle n))))
