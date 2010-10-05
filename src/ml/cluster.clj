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

(defn kmeans
  [k mat]
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

  (defn rands-unq
    [length n used]
    (if (= (count used) n)
      used
      (let [x (rand-int length)]
        (if (some (fn [y] (= x y)) used)
          (rands-unq length n used)
          (rands-unq length n (concat used [x]))))))

  (defn expect
    [points means]
    (map (fn [x] (for [j x] (last j)))
      (partition-by
        (fn [x] (first x))
        (sort-by (fn [x] (first x))
          (for [point points]
            [(nearest-cluster point means) point])))))

  (defn iter
    [mat means]
    (println means)
    (let [clusters (expect mat means)
          means-new (map centroid clusters)]
      (if (= means-new means)
        clusters
        (iter mat means-new))))

  (iter mat (sel mat :rows (rands-unq (count mat) k []))))

(defn kmeans-sim
  []
  (kmeans 3
    (matrix (concat (data-gen 2 4 9 0.5)
                    (data-gen 2 4 0 0.5)
                    (data-gen 2 4 -5 0.5)))))

(defn kmeans-plot
  []
  (let [res (kmeans-sim)
        groups (mapcat identity
                 (for [i (range (count res))]
                   (repeat (count (nth res i)) i)))
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
  (kmeans-sim))
