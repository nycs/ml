(ns ml.cluster
  (:use [incanter core stats charts]))

(defn chart-normal
  [size mean sd]
  (histogram 
    (sample-normal size :mean mean :sd sd)
    :title "Normal Sample"
    :x-label (str "sample-size = " size
                  ", mean = " mean
                  ", sd = " sd)))
