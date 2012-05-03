;Problem 89
;Starting with a graph you must write a function that returns true if 
;it is possible to make a tour of the graph in which ;every edge is 
;visited exactly once.

;The graph is represented by a vector of tuples, where each tuple represents a single edge.

;The rules are:

;- You can start at any node.
;- You must visit each edge exactly once.
;- All edges are undirected.
(fn [edges]
  (letfn
    [(connected-vertices
       [connected-vertices edge]
       (if (= (first edge) (second edge))
         connected-vertices
         (let [edge (set edge)
               new (filter
                     (complement
                       #(empty? (#'clojure.set/intersection edge %)))
                     connected-vertices)]
           (into
             (#'clojure.set/difference connected-vertices (set new))
             #{(reduce #'clojure.set/union edge new)}))))
     (degree
       [degrees edge]
       (assoc
         (assoc
           degrees
           (first edge)
           (if (get degrees (first edge))
             (inc (get degrees (first edge)))
             1))
         (second edge)
         (if (get degrees (second edge))
           (inc (get degrees (second edge)))
           1)))]
    (let [result (reduce
                   (fn [[conn-vertices degrees] edge] [(connected-vertices
                                                              conn-vertices
                                                              edge)
                                                            (degree
                                                              degrees
                                                              edge)])
                   [#{} {}]
                   edges)
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