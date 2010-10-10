(ns ml.cluster
  (:use [incanter core stats charts]))

(defn sim
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
