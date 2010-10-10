(ns ml.core
  (:use [compojure.core]
        [compojure.response]
        [ml.cluster.kmeans :as kmeans]
        [ml.cluster :as cluster]
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

(def sample-form
  (html-doc "sample-normal histogram"
    (form-to [:get "/sample-normal"]
      "sample size: " (text-field {:size 4} :size)
      "mean: " (text-field {:size 4} :mean)
      "sd: " (text-field {:size 4} :sd)
      (submit-button " view"))))

(defn gen-samp-hist-png [request size-str mean-str sd-str]
    (defn chart-normal
      [size mean sd]
      (histogram 
        (sample-normal size :mean mean :sd sd)
        :title "Normal Sample"
        :x-label (str "sample-size = " size
                      ", mean = " mean
                      ", sd = " sd)))
    (let [size (if (nil? size-str)
                 1000
                 (Integer/parseInt size-str))
          m (if (nil? mean-str)
              0
              (Double/parseDouble mean-str))
          s (if (nil? sd-str)
              1
              (Double/parseDouble sd-str))]
          {:status 200
           :headers {"Content-Type" "image/png"}
           :body (pnger (chart-normal size m s))}))

(defroutes webservice
  (GET "/" [] sample-form)
  (GET "/cluster/kmeans" [] {:status 200
                             :headers {"Content-Type" "image/png"}
                             :body (pnger
                                     (cluster/plot
                                       (cluster/sim kmeans/alg)))})
  (GET "/sample-normal"
    {request :request, params :params}
      (gen-samp-hist-png request (params "size") (params "mean") (params "sd")))
  (route/not-found "this page wasn't found"))

(def service-wrapper
  (wrap-reload #'webservice '(ml.core ml.cluster.kmeans)))

(defn serve []
  (run-jetty #'service-wrapper {:port 8080}))
