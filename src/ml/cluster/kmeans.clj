(ns ml.cluster.kmeans
  (:use [incanter core stats]
        [ml.math]))

(defn- closest-neighbor
  "Relative to the provided home point, find the closest point from
  among the seq of neighboring coordinates."
  [neighbors home]
  (first
    (sort-by (partial euclidean-distance home) (apply list neighbors))))

(defn- expect
  "Runs the expectation step in the k-means algorithm. That is,
  it groups the points by their closest mean."
  [points means]
  (vals
    (group-by
      #(into [] (closest-neighbor means %))
      points)))

(defn run
  "Partitions a matrix of two-dimensional points into k clusters using
  the k-means algorthm. This is accomplished by iterating through the
  k-means algorithm until it converges on a solution, i.e. when
  the means of the clusters found by the current iteration are equal to
  those of the previous iteration."
  ([k mat]
    (run k mat (sel mat :rows (rands-unq (count mat) k))))
  ([k mat means]
    (let [clusters (expect mat means)
          means-new (map centroid clusters)]
      (if (= means-new means)
        clusters
        (run k mat means-new)))))
