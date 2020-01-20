(ns scrabble-solver.params
  (:use [scrabble-solver.tree-dic]))

(def letter-values
  {\a 1 \b 5 \c 3 \d 3 \e 1 \f 5 \g 5 \h 5 \i 1 \j 10 \k 10 \l 2 \m 4 \n 1 \o 1 \p 4 \q 10 \r 1 \s 1 \t 1 \u 2 \v 8 \w 10 \x 10 \y 10 \z 8})

(def bonuses
  (hash-map
    [0 0] :l3 [2 0] :w3 [8 0] :w3 [10 0] :l3
    [1 1] :w2 [5 1] :w2 [9 1] :w2
    [0 2] :w3 [2 2] :l3 [4 2] :l2 [6 2] :l2 [8 2] :l3 [10 2] :w3
    [3 3] :l3 [7 3] :l3
    [2 4] :l2 [8 4] :l2
    [1 5] :w2 [9 5] :w2
    [2 6] :l2 [8 6] :l2
    [3 7] :l3 [7 7] :l3
    [0 8] :w3 [2 8] :l3 [4 8] :l2 [6 8] :l2 [8 8] :l3 [10 8] :w3
    [1 9] :w2 [5 9] :w2 [9 9] :w2
    [0 10] :l3 [2 10] :w3 [8 10] :w3 [10 10] :l3))

(def available-letters
  {\a 6 \b 1 \c 1 \d 1 \e 7 \f 1 \g 1 \h 1 \i 4 \j 1 \k 1 \l 2 \m 1 \n 3 \o 4 \p 1 \q 1 \r 3 \s 4 \t 3 \u 2 \v 1 \w 1 \x 1 \y 1})

(def valid-words
  (filter #(<= (count %) 11)
          (with-open [rdr (clojure.java.io/reader "resources/words")]
            (map clojure.string/lower-case (into [] (line-seq rdr))))))

(def word-dic
  (build-dic valid-words))

(def valid-words-set
  (into #{} valid-words))
