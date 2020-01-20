(ns scrabble-solver.client
  (:require [org.httpkit.client :as http]))

@(http/get "http://localhost:8090/find-solutions"
           {:query-params {:available-letters "owefih"
                           :board []}})
