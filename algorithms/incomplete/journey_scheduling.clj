(defn get-connections [roads]
  (let [unique-cities (set (flatten roads))
        connections   (apply hash-map (concat (interpose #{} unique-cities) [#{}]))
        assoc-two     (fn [city1 city2 conn]
                        (let [cities->city1 (get conn city1)
                              cities->city2 (get conn city2)
                              c1-added (assoc conn city1 
                                              (conj cities->city1 city2))
                              c1c2-added (assoc c1-added city2 
                                                (conj cities->city2 city1))]
                          c1c2-added))]
    (loop [roads roads
           conn  connections]
      (if (empty? roads)
        conn
        (let [next-road  (first roads)
              c1         (first  next-road)
              c2         (second next-road)
              new-conn   (assoc-two c1 c2 conn)]
          (recur (rest roads) new-conn))))))

(def n&m (let [[n m] (clojure.string/split (read-line) #" ")]
           [(Integer/parseInt n) (Integer/parseInt m)]))
 
(def roads 
  (loop [to-collect (dec (first n&m))
         acc        []]
    (if (zero? to-collect)
      acc
      (let [[c1 c2] (clojure.string/split (read-line) #" ")
            new-acc     (conj acc [(Integer/parseInt c1) (Integer/parseInt c2)])]
        (recur (dec to-collect) new-acc)))))
 
(def connections (get-connections roads))

(def journeys (doall (for [i (range (second n&m))]
                (let [[start end] (clojure.string/split (read-line) #" ")]
                  [(Integer/parseInt start) (Integer/parseInt end)]))))

(defn in? [seq el]
  (some #(= el %) seq))

(defn get-connected-cities [city]
  (vec (get connections city)))

(defn explore-branch [curr-city prev-city trav-dist unexplored-stack visited]
  (let [connected-cities          (get-connected-cities curr-city)
        relevant-connected-cities (filter #(not (or (= prev-city %)     ;going backwards
                                                    (in? visited %)))   ;going in cycles
                                            connected-cities)
        next-city                 (first relevant-connected-cities)
        other-conn-cities         (rest  relevant-connected-cities)]
    (if (nil? next-city)
      [{:name curr-city :distance trav-dist} unexplored-stack]
      (let [new-visited          (conj visited next-city)
            new-trav-dist        (inc trav-dist)
            fn-tmpl              (fn [other-next-city]
                                   #(explore-branch other-next-city curr-city
                                                    new-trav-dist [] new-visited))
            new-unexplored-stack (concat unexplored-stack 
                                         (map fn-tmpl other-conn-cities))]
        (recur next-city curr-city new-trav-dist new-unexplored-stack new-visited)))))

(defn get-leaf-cities [curr-city]
  (let [[first-leaf-city init-unexplored-stack] (explore-branch curr-city nil 0 [] [])]
    (loop [leaf-cities [first-leaf-city]
           stack       init-unexplored-stack]
      (let [[new-leaf-city
             unexplored-paths] ((first stack))
             new-leaf-cities   (conj leaf-cities new-leaf-city)
             new-stack         (concat (rest stack) unexplored-paths)]
        (if (empty? new-stack)
          new-leaf-cities
          (recur new-leaf-cities new-stack))))))

(def furthest-destinations
  (memoize
   (fn [current-city]
     (let [leaf-cities        (get-leaf-cities current-city)
           sorted-leaf-cities (reverse (sort-by :distance leaf-cities))
           furthest-distance  (:distance (first sorted-leaf-cities))]
       (take-while #(= (:distance %) furthest-distance) sorted-leaf-cities)))))

(defn least-visited-destinations [destinations visitations]
    (let [dest-names (map :name destinations)
          relevant-visitations (filter #(in? dest-names 
                                             (first %)) 
                                       visitations)]
      (if (= (count relevant-visitations) 0)
        destinations
        (let [sorted-visitations   (sort-by val < relevant-visitations)
              smallest-nr-visits   (val (first sorted-visitations))
              least-visited        (take-while #(= smallest-nr-visits (val %1))
                                               sorted-visitations)
              least-visited-dest   (filter #(= (first (first least-visited))
                                               (:name %))
                                           destinations)]
          least-visited-dest))))

(defn decide-destination [curr-city visitations]
  (let [destinations (furthest-destinations curr-city)]
    (if (= (count destinations) 1)
      (first destinations)
      (let [destinations (least-visited-destinations destinations visitations)]
        (if (= (count destinations) 1)
          (first destinations)
          (let [destination (first (sort-by :name destinations))]
            destination))))))

(defn journey-scheduling [curr-city to-visit visited-cities first-journey-dist]
  (let [destination         (decide-destination curr-city visited-cities)
        dest-name           (:name destination)
        new-visited-cities  (if (nil? (get visited-cities dest-name))
                              (assoc visited-cities dest-name 1)
                              (update-in visited-cities [dest-name] inc))
        first-journey-dist  (if (= first-journey-dist 0)
                                (:distance destination)
                                first-journey-dist)]
    (if (= 2 (count new-visited-cities))
      (let [distances (:distance destination)
            to-travel (+ first-journey-dist (* distances (- to-visit 1)))]
        to-travel)
      (recur (:name destination) to-visit new-visited-cities first-journey-dist))))

(defn solve [tc]
  (let [start-city (first tc)
        nr-visits  (second tc)
        answer (journey-scheduling start-city nr-visits {} 0)]
    (println answer)))

(doall (map solve journeys))

