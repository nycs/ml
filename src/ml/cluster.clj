(ns ml.cluster
  (:use [incanter core stats charts]))

(defn data-gen
  [cols rows mean sd]
  (for [x (range rows)] (sample-normal cols :mean mean :sd sd)))

(defn chart-normal
  [size mean sd]
  (histogram 
    (sample-normal size :mean mean :sd sd)
    :title "Normal Sample"
    :x-label (str "sample-size = " size
                  ", mean = " mean
                  ", sd = " sd)))

(defn distances
  [points point]
  (for [i (count points)]
    [(euclidean-distance point (nth points i)) i]))

(defn min-distance
  [point means i curmin]
  (if (>= i (count means))
    curmin
    (let [dist (euclidean-distance point (nth means i))]
      (if (or (nil? curmin) (< dist (last curmin)))
        (min-distance point means (+ 1 i) [i dist])
        (min-distance point means (+ 1 i) curmin)))))

(defn nearest-cluster
  [point means]
  (first (min-distance point means 0 nil)))

(defn centroid
  [points]
  (map (fn [x] (/ (sum x) (count x)))
    (for [i (range (count (first points)))] 
      (map (fn [x] (nth x i)) points))))

(defn expect
  [points means]
  (map (fn [x] (for [j x] (last j)))
    (partition-by
      (fn [x] (first x))
      (sort-by (fn [x] (first x))
        (for [point points]
          [(nearest-cluster point means) point])))))

(defn maximize
  [clusters]
  (map centroid clusters))

(defn kmeans-iter
  [k mat means]
  (let [clusters (expect mat means)
        means-new (map centroid clusters)]
    (if (= means-new means)
      clusters
      (kmeans-iter k mat means-new))))

(defn kmeans
  [k mat]
  (kmeans-iter k mat (sel mat :rows [0 3 6])))

(defn kmeans-test
  []
  (let [res (kmeans 3
              (matrix (concat (data-gen 2 3 -10 0.5)
                              (data-gen 2 3 0 0.5)
                              (data-gen 2 3 10 0.5))))
        groups [0 0 0 1 1 1 2 2 2]
        x (mapcat 
            (fn [z]
              (for [i z] (first i)))
            res)
        y (mapcat 
            (fn [z]
              (for [i z] (last i))
              )
            res)]
    (scatter-plot x y :group-by groups)))

(defn -main [& args]
  (kmeans-test))
