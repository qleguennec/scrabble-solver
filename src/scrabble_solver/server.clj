(ns scrabble-solver.server
  (:require [org.httpkit.server :as http]
            [clj-time.core :as t]
            [clojure.data.json :as json])
  (:use [scrabble-solver.solver :as solve]
        [scrabble-solver.play]
        [compojure.route :only [files not-found]]
        [compojure.core :only [defroutes GET POST DELETE ANY context]]
        [clojure.pprint]))

(defn home-handler [req]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (str (t/time-now))})

(defn find-solutions-handler
  [req]
  {:status 200
   :headers {"Content-Type" "text/json"}
   :body (json/write-str
           (solve/find-solutions (-> req :params :board)
                                 (-> req :params :available-letters)))})

(defn draw-handler
  [req]
  {:status 200
   :headers {"Content-Type" "text/json"}
   :body (draw (-> req :params :current-draw)
               (-> req :params :available-letters))})

(defroutes router
  (GET "/" [] home-handler)
  (GET "/find-solutions" [] find-solutions-handler)
  (GET "/draw" [] draw-handler))

