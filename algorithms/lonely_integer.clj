(let [_ (read-line)
    arr (clojure.string/split (read-line) #" ")
    answer (#(-> % first first)
                (filter #(= (second %) 1)
                            (frequencies arr)))]
    (println answer))
