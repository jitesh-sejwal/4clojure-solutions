;Levenshtein Distance
 
;Difficulty:	Hard
;Topics:	seqs


;Given two sequences x and y, calculate the Levenshtein distance 
;of x and y, i. e. the minimum number of edits needed to transform x into y. The allowed edits are:

;- insert a single item
;- delete a single item
;- replace a single item with another item

;WARNING: Some of the test cases may timeout if you write an inefficient solution!

(fn [s t]
  (last
    (last
      (reduce
        (fn [acc x]
          (conj
            acc
            (reduce
              (fn [acc2 y]
                (let [n (dec (count acc)) m (count acc2)]
                  (conj
                    acc2
                    (if (= x y)
                      ((acc n) (dec m))
                      (inc
                        (min
                          ((acc n) (dec m))
                          (acc2 (dec m))
                          ((acc n) m)))))))
              [(count acc)]
              s)))
        [(vec (range (inc (count s))))]
        t))))