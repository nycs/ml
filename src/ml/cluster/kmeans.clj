(ns ml.cluster.kmeans
  (:use [incanter core stats]
        [ml.math]))

(defn- closest-neighbor
  "Relative to the provided home point, find the closest point from
  among the seq of neighboring coordinates."
  [home neighbors i curmin]
  (if (>= i (count neighbors))
    curmin
    (let [dist (euclidean-distance home (nth neighbors i))]
      (if (or (nil? curmin) (< dist (last curmin)))
        (min-distance home neighbors (+ 1 i) [i dist])
        (min-distance home neighbors (+ 1 i) curmin)))))

(defn- nearest-cluster
  "Returns the cluster (index of the mean point in
  the means seq) that is nearest to the given point."
  [point means]
  (first (min-distance point means 0 nil)))

(defn- expect
  "Runs the expectation step in the k-means algorithm."
  [points means]
  (map (fn [x] (for [j x] (last j)))
    (partition-by
      (fn [x] (first x))
      (sort-by (fn [x] (first x))
        (for [point points]
          [(nearest-cluster point means) point])))))

(defn- iter
  "Iterates through the k-means algorithm until a suitable solution is found."
  [mat means]
  (let [clusters (expect mat means)
        means-new (map centroid clusters)]
    (if (= means-new means)
      clusters
      (iter mat means-new))))

(defn alg
  "Solves k-means for a given number of clusters and matrix of points."
  [k mat]
  (println mat)
  (iter mat (sel mat :rows (rands-unq (count mat) k []))))
