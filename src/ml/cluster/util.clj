(ns ml.cluster.util
  (:use [incanter core stats charts]))

(defn sim
  "Run a clustering simulation. Executes the given algorithm with 3 clusters
  and a random set of 12 points."
  [alg]
  (defn data-gen
    [cols rows mean sd]
    (for [x (range rows)] (sample-normal cols :mean mean :sd sd)))

  (alg 3
    (matrix
      (shuffle
        (concat (data-gen 2 4 9 0.5)
                (data-gen 2 4 0 0.5)
                (data-gen 2 4 -5 0.5))))))

(defn plot
  "Plot some clusters."
  [clusters]
  (let [groups (flatten
                 (for [i (range (count clusters))]
                   (repeat (count (nth clusters i)) i)))
        x (mapcat 
            (fn [z]
              (for [i z] (first i)))
            clusters)
        y (mapcat 
            (fn [z]
              (for [i z] (last i))
              )
            clusters)]
      (scatter-plot x y :group-by groups)))
