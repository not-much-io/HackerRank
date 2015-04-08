(ns journey-scheduling)

; Utility functions

(defn- get-connections [roads]
  "Associate all cities with a set of the cities they are connected to."
  (let [unique-cities (set (flatten roads))
        connections   (apply hash-map (concat (interpose #{} unique-cities) [#{}]))
        assoc-two     (fn [city1 city2 conn]
                        (let [cities->city1 (get conn city1)
                              cities->city2 (get conn city2)
                              c1-added (assoc conn city1 (conj cities->city1 city2))
                              c1c2-added (assoc c1-added city2 (conj cities->city2 city1))]
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

(defn in? [seq el]
  (some #(= el %) seq))

; Input

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

; Logic

(defn get-connected-cities [city]
  (vec (get connections city)))

(defn- inc-values [m]
  (apply merge
         (map (fn [[k v]] {k (inc v)}) m)))

(defn- dec-values [m]
  (apply merge
         (map (fn [[k v]] {k (dec v)}) m)))


(def most-distants (atom {}))

(defn explore-branch [curr-city prev-city unexplored-stack visited]
  "Explores a road to a single leaf city and returns the distance form all 
   the traveled through cities to that leaf city. Also returns a stack of 
   functions to call to explore the rest of the branch"
  (let [connected-cities          (get-connected-cities curr-city)
        relevant-connected-cities (filter #(not (or (= prev-city %); backwards
                                                    (in? (keys visited) %))); in cycles
                                          connected-cities)
        next-city                 (first relevant-connected-cities)
        other-conn-cities         (rest  relevant-connected-cities)]
    (if (nil? next-city)
      (do (for [[city dist] visited] 
            (if (> dist (get @most-distants city))
              (swap! most-distants a)))
          unexplored-stack) ; dec-values quick patch
      (let [new-visited          (inc-values (assoc visited next-city 0))
            fn-tmpl              (fn [other-next-city]
                                   #(explore-branch other-next-city 
                                                    curr-city
                                                    []
                                                    (inc-values 
                                                     (assoc visited other-next-city 0))))
            new-unexplored-stack (concat unexplored-stack 
                                         (map fn-tmpl other-conn-cities))]
        (recur next-city curr-city new-unexplored-stack new-visited)))))


(defn- build-tree-structure [leaf-cities-and-distances-from-nodes]
  "From a sequence of node to leaf city distance associations."
  (let [leaf-cities                 (map first leaf-cities-and-distances-from-nodes)
        nr-leaf-cities              (count leaf-cities)
        empty-tree                  (apply merge
                                           (map hash-map leaf-cities 
                                                (repeat nr-leaf-cities {})))]
    (loop [lfcities-and-dists leaf-cities-and-distances-from-nodes
           tree               empty-tree]
      (let [curr-assoc (first lfcities-and-dists)
            lf-city    (first curr-assoc)
            new-dists  (second curr-assoc)
            curr-dists (get tree lf-city)
            new-tree   (assoc tree lf-city (merge curr-dists
                                                  new-dists))]
        (if (empty? (rest lfcities-and-dists))
          new-tree
          (recur (rest lfcities-and-dists) new-tree))))))



(defn explore-tree [curr-city]
  "Will return all the cities and the distance to furthest leaf cities from them."
  (let [[init-nodes->dist-leaf-city init-unexplored-stack] 
        (explore-branch curr-city nil [] {curr-city 1})]
    (loop [nodes->dist-leaf-cities [init-nodes->dist-leaf-city]
           stack              init-unexplored-stack]
      (let [[new-nodes->dist-leaf-city
             unexplored-paths]           ((first stack))
             new-nodes->dist-leaf-cities (conj nodes->dist-leaf-cities 
                                               new-nodes->dist-leaf-city)
             new-stack                   (concat (rest stack) unexplored-paths)]
        (if (empty? new-stack)
          (build-tree-structure new-nodes->dist-leaf-cities)
          (recur new-nodes->dist-leaf-cities new-stack))))))

(def leaf-cities-and-distances (explore-tree (first (first roads))))

(defn furthest-destination [city]
  "Returns the furthest city from this the input city."
  (get distances-to-furthest-cities city))

(defn journey-scheduling [start-city to-visit]
  (let [dest1 (furthest-destination start-city)
        dest2 (furthest-destination dest1)]
    (let [dist1 (:distance dest2)
          dist2 (:distance dest2)]
      (+ dist1 (* dist2 (- to-visit 1))))))

(defn solve [tc]
  (let [start-city (first tc)
        nr-visits  (second tc)
        answer (journey-scheduling start-city nr-visits)]
    (println answer)))

(doall (map solve journeys))










