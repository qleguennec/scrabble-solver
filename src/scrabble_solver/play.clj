(ns scrabble-solver.play
  (:require [scrabble-solver.solver :as solve])
  (:use [scrabble-solver.tree-dic]
        [scrabble-solver.util]
        [scrabble-solver.solver]
        [scrabble-solver.params]
        [clojure.pprint]))

(defn play [players]
  ((fn [{:keys [board draw turn available-letters] :as state}]
     (let [nplayer (mod turn (count players))
           current-player (nth players nplayer)
           player-draw (nth draw nplayer)
           player-new-draw (apply str (concat player-draw (take (- 7 (count player-draw))
                                                                available-letters)))]

       (if-let [{:keys [x y direction word letters-used] :as solution}
                ((:strategy current-player) (find-solutions board player-new-draw))]

         (recur {:board (cons (assoc solution :player (:name current-player)) board)
                 :turn (inc turn)
                 :draw (assoc draw nplayer (freq-to-str (merge-with -
                                                                    (full-freq player-new-draw)
                                                                    (full-freq letters-used))))
                 :available-letters (drop (- 7 (count player-draw)) available-letters)})

         state)))

   {:board '()
    :turn 0
    :draw (into [] (repeat (count players) ""))
    :available-letters (shuffle (freq-to-str available-letters))}))

(defn get-total-score [{:keys [board]}]
  (->> board
       (group-by :player)
       (reduce-kv (fn [m k v] (assoc m k (reduce + (map :score v)))) {})))

(defn draw [current-draw available-letters]
  [(concat current-draw (take (- 7 (count current-draw))))
   (drop (take (- 7 (count current-draw))) available-letters)])
