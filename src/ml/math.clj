(ns ml.math)

(defn centroid
  "Calculates the centroid of a list of euclidean points."
  [points]
  (map #(/ (reduce + %) (count %))
    (for [i (range (count (first points)))] 
      (map #(nth % i) points))))

(defn rands-unq
  "Returns a random selection of n unique numbers in the range 0 to max.
  Any numbers in the exclude seq won't be selected."
  [max n exclude]
  (if (= (count exclude) n)
    exclude
    (let [x (rand-int max)]
      (if (some #(= x %) exclude)
        (rands-unq max n exclude)
        (rands-unq max n (concat exclude [x]))))))
