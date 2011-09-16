(ns ml.core
  (:use [compojure.core]
        [compojure.response]
        [compojure.handler]
        [ml.cluster.kmeans :as kmeans]
        [ml.cluster.util :as util]
        [ring.util.servlet]
        [ring.adapter.jetty]
        [ring.middleware.reload]
        [ring.middleware.stacktrace]
        [incanter core charts stats]
        [hiccup core form-helpers page-helpers])
  (:require [compojure.route :as route])
  (:import (java.io ByteArrayOutputStream
                    ByteArrayInputStream)))

(defn pnger
  [chart]
  (let [out-stream (ByteArrayOutputStream.)
        in-stream (do
                    (save chart out-stream)
                    (ByteArrayInputStream.
                    (.toByteArray out-stream)))]
    in-stream))

(defn html-doc 
  "From the example from the incanter docs."
  [title & body] 
  (html 
    (doctype :html4) 
    [:html 
      [:head 
        [:title title]] 
      [:body 
        [:div 
          [:h2 
            [:a {:href "/"} 
             "Generate a normal sample"]]]
        body]]))

(defn sample-form
  "From the example from the incanter docs."
  []
  (html-doc "sample-normal histogram"
    (form-to [:get "/sample-normal"]
      "sample size: " (text-field {:size 4} :size)
      "mean: " (text-field {:size 4} :mean)
      "sd: " (text-field {:size 4} :sd)
      (submit-button " view"))))

(defn gen-samp-hist-png
  "Slightly modified from the example from the incanter docs."
  [request size-str mean-str sd-str]
  (defn chart-normal
    [size mean sd]
    (histogram 
      (sample-normal size :mean mean :sd sd)
      :title "Normal Sample"
      :x-label (str "sample-size = " size
                    ", mean = " mean
                    ", sd = " sd)))
  (let [size (if size-str (Integer/parseInt size-str) 1000)
        m (if mean-str (Double/parseDouble mean-str) 0)
        s (if sd-str (Double/parseDouble sd-str) 1)]
        {:status 200
         :headers {"Content-Type" "image/png"}
         :body (pnger (chart-normal size m s))}))

(site
  (defroutes ml-routes
    (GET "/" [] sample-form)
    (GET "/cluster/kmeans" [] {:status 200
                               :headers {"Content-Type" "image/png"}
                               :body (pnger
                                       (util/plot
                                         (util/sim kmeans/run)))})
    (GET "/sample-normal"
      {request :request, params :params}
        (gen-samp-hist-png request (params "size") (params "mean") (params "sd")))
    (route/not-found "this page wasn't found")))

(def service-wrapper
  (wrap-reload #'ml-routes '(ml.core ml.math ml.cluster.util ml.cluster.kmeans)))

(defn serve []
  (run-jetty service-wrapper {:port 8080}))

;(cluster/sim kmeans/alg)
