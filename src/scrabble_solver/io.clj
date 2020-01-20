(ns scrabble-solver.io
  (:require [monger.core :as mg]
            [monger.collection :as mc]
            [monger.query :refer :all])
  (:use [scrabble-solver.play]))

(def conn (mg/connect))
(def db (mg/get-db conn "scrabble"))

(defn populate-game-db [players]
  (repeatedly #(mc/insert db "games" (merge {:players (map :name players)} (play players)))))

(defn find-average-player-score [players]
  (->> (mq/with-collection db "games"
         (find {:$or [{:players (map :name players)}
                      {:players (reverse (map :name players))}]})
         (fields [:board]))
       (map :board)
       (flatten)
       (group-by :player)
       (reduce-kv (fn [m k v] (assoc m k (float (/ (reduce + (map :score v)) (count v))))) {})))
