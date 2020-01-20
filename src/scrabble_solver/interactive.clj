(ns scrabble-solver.interactive
  (:use [scrabble-solver.solver]
        [scrabble-solver.util]))

(def other
  '({:x 5 :y 5 :word "opales" :direction :bottom}
    {:x 2 :y 10 :word "bens" :direction :right}))

(def board
  '())

(def jerome
  '({:x 1 :y 5 :word "quais" :direction :right}
    {:x 5 :y 0 :word "ornees" :direction :bottom}
    {:x 4 :y 0 :word "colon" :direction :right}
    {:x 10 :y 1 :word "delitees" :direction :bottom}
    {:x 3 :y 2 :word "panseuse" :direction :right}
    {:x 2 :y 6 :word "si" :direction :right}
    {:x 2 :y 4 :word "wus" :direction :bottom}
    {:x 0 :y 8 :word "mage" :direction :right}
    {:x 6 :y 6 :word "ray" :direction :bottom}
    {:x 3 :y 7 :word "ratai" :direction :right}
    {:x 9 :y 7 :word "hun" :direction :bottom}
    {:score 35,
     :x 0,
     :y 8,
     :direction :bottom,
     :word "mox",
     :letters-used "ox",
     :potential-next-word
     {:score 33,
      :x 7,
      :y 7,
      :direction :right,
      :word "kote",
      :letters-used "kot"},
     :combined-score 68}
    {:x 8 :y 5 :word "jet" :direction :right}
    {:x 2 :y 8 :word "gaz" :direction :bottom}))

(def vanessa
  '({:score 3,
     :x 5,
     :y 5,
     :direction :right,
     :word "eons",
     :letters-used "eno",
     :potential-next-word
     {:score 5,
      :x 3,
      :y 7,
      :direction :right,
      :word "eon",
      :letters-used "eo"},
     :combined-score 8}
    {:score 78,
     :x 2,
     :y 5,
     :direction :bottom,
     :word "expira",
     :letters-used "aeipx",
     :potential-next-word
     {:score 10,
      :x 5,
      :y 1,
      :direction :bottom,
      :word "aorte",
      :letters-used "ar"},
     :combined-score 88}
    {:x 8 :y 5 :word "sinuee" :direction :bottom}
    {:x 2 :y 9 :word "relu" :direction :right}
    {:x 6 :y 10 :word "ame" :direction :right}
    {:x 6 :y 4 :word "soc" :direction :bottom}
    {:x 3 :y 4 :word "qats" :direction :right}
    {:x 6 :y 8 :word "faute" :direction :right}
    {:x 4 :y 2 :word "volant" :direction :right}
    {:x 10 :y 0 :word "wus" :direction :bottom}
    {:x 7 :y 0 :word "show" :direction :right}))

(find-solutions
  board
  "")

(read-board
  (build-tiles-letter-mapping vanessa))

