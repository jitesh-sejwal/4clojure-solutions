;Starting with a graph you must write a function that returns true if 
;it is possible to make a tour of the graph in which ;every edge is 
;visited exactly once.

;The graph is represented by a vector of tuples, where each tuple represents a single edge.

;The rules are:

;- You can start at any node.
;- You must visit each edge exactly once.
;- All edges are undirected.

(fn [x]
  (letfn
    [(connected
       [edges]
       (reduce
         (fn [[connected-vertices count] edge]
           (do
             (println connected-vertices count edge)
             [(if (= (first edge) (second edge))
                connected-vertices
                (let [edge (set edge)
                      new (filter
                            (complement
                              #(empty?
                                (#'clojure.set/intersection edge %)))
                            connected-vertices)]
                  (into
                    (#'clojure.set/difference
                      connected-vertices
                      (set new))
                    #{(reduce #'clojure.set/union edge new)})))
              (assoc
                (assoc
                  count
                  (first edge)
                  (if (get count (first edge))
                    (inc (get count (first edge)))
                    1))
                (second edge)
                (if (get count (second edge))
                  (inc (get count (second edge)))
                  1))]))
         [#{} {}]
         edges))]
    (let [result (connected x)
          vertices (first result)
          counts (vals (second result))]
      (do
        (println result)
        (and
          (if (or
                (every? even? counts)
                (= 2 (count (filter odd? counts))))
            true
            false)
          (= 1 (count vertices)))))))