(ns scrabble-solver.core
  (:require [org.httpkit.server :as http])
  (:use [scrabble-solver.server :only [router]]
        [ring.middleware.defaults :refer :all]))

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn -main [& args]
  (stop-server)
  (reset! server (http/run-server
                   (wrap-defaults router api-defaults)
                   {:port 8090})))
