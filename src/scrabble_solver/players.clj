(ns scrabble-solver.players)

(defmacro defplayer [name & strategy]
  `(def ~(symbol name)
     {:name ~(str name)
      :strategy (fn [~'solutions] ~@strategy)}))

(defplayer max-score
  (first solutions))

(defplayer max-score-randomized
  (some->> solutions
           (partition-by :score)
           (sort-by (comp :score first) >)
           (first)
           (rand-nth)))

(defplayer random
  (if (empty? solutions)
    nil
    (rand-nth solutions)))
