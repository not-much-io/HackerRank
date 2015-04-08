(ns journey-scheduling-alt)

(defn- double-conj [v1 v2 conn]
  "Adds v1 to v2s values in conn and v2 to v1s value.
   
   Helper for: get-connections"
  (let [values-v1 (get conn v1)
        values-v2 (get conn v2)
        v1-added      (assoc conn v1 (conj values-v1 v2))]
    (assoc v1-added v2 (conj values-v2 v1))))

(defn- build-connection-structure [roads conn]
  "Take all the roads and an empty connection structure 
   (with keys being all distinct cities and values empty sets) and
   adds the cities that are connected to each city into their respective
   sets.

   Helper for: get-connections"
  (if (empty? roads)
    conn
    (let [next-road  (first roads)
          c1         (first  next-road)
          c2         (second next-road)
          new-conn   (double-conj c1 c2 conn)]
      (recur (rest roads) new-conn))))

(defn get-connections [roads]
  "Associate all cities with a set of the cities they are connected to."
  (let [unique-cities     (set (flatten roads))
        empty-connections (apply hash-map 
                                 (concat (interpose #{} unique-cities) [#{}]))]
    (build-connection-structure roads empty-connections)))

(defn in? [seq el]
  (some #(= el %) seq))

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

(defn get-connected-cities [city]
  (vec (get connections city)))

(def branch-len
  (atom {}))

(defn- update-branch-len [dist root]
  (let [curr-dist (get @branch-len root)]
    (if (or (nil? curr-dist) (> dist curr-dist))
      (swap! branch-len assoc root dist))))

(defn explore-branch [curr-city prev-city unexplored-stack visited dist root]
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
      (do
        (update-branch-len dist root)
        unexplored-stack)             ; dec-values quick patch
      (let [new-visited          (apply-to-vals inc (assoc visited next-city 0))
            fn-tmpl              (fn [other-next-city]
                                   #(explore-branch other-next-city 
                                                    curr-city
                                                    []
                                                    (inc-values 
                                                     (assoc visited other-next-city 0))
                                                    dist
                                                    root))
            new-unexplored-stack (concat unexplored-stack 
                                         (map fn-tmpl other-conn-cities))]
        (recur next-city curr-city new-unexplored-stack new-visited (inc dist) root)))))

(defn explore-tree [curr-city]
  "Will return all the cities and the distance to furthest leaf cities from them."
  (let [root-cities           (get-connected-cities curr-city)
        explore-call          #(explore-branch % curr-city [] {% 1} 0 %)
        init-unexplored-stack (flatten 
                               (map explore-call root-cities))]
    (loop [stack                   init-unexplored-stack]
      (let [unexplored-paths           ((first stack))
            new-stack                   (concat (rest stack) unexplored-paths)]
        (if (empty? new-stack)
          @most-distants
          (recur new-stack))))))


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

(time (explore-tree (first (first roads))))
(doall (map solve journeys))
