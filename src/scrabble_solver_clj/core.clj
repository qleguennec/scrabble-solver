(ns scrabble-solver-clj.core
  (:require [clojure.set :as set]
            [clojure.core.reducers :as r]
            [clojure.string :as s])
  (:use [scrabble-solver-clj.tree-dic]))

(def debug-on true)

(def letter-values
  {\a 1 \b 3 \c 3 \d 2 \e 1 \f 4 \g 2 \h 5 \i 1 \j 8 \k 10 \l 1 \m 2 \n 1 \o 1 \p 3 \q 10 \r 1 \s 1 \t 1 \u 2 \v 8 \w 10 \x 10 \y 10 \z 10})

(def bonuses
  (hash-map
    [0 0] :l3 [2 0] :w3 [8 0] :w3 [10 0] :l3 [1 1] :w2 [5 1] :w2 [9 1] :w2 [0 2] :w3 [2 2] :l3 [4 2] :l2 [6 2] :l2 [8 2] :l3 [10 2] :w3 [3 3] :l3 [7 3] :l3 [2 4] :l2 [8 4] :w2 [1 5] :w2 [9 5] :l2 [2 6] :l2 [8 6] :l3 [3 7] :l3 [7 7] :l3 [0 8] :l3 [2 8] :l2 [4 8] :l2 [6 8] :l2 [8 8] :l3 [10 8] :w3 [1 9] :w2 [5 9] :w2 [9 9] :w2 [0 10] :l3 [2 10] :w3 [8 10] :w3 [10 10] :l3))

(def available-letters
  {\a 5 \b 1 \c 1 \d 2 \e 7 \f 1 \g 1 \h 1 \i 4 \j 1 \k 1 \l 2 \m 1 \n 2 \o 4 \p 1 \q 1 \r 2 \s 4 \t 2 \u 1 \v 1 \w 1 \x 1 \y 1 \z 1 \? 2})

(defn opposite-direction
  [direction]
  (if (= direction :right)
    :bottom :right))

(defn if-right
  [direction then else]
  (if (= direction :right)
    then
    else))

(defn tiles-taken
  [{:keys [x y direction width word]}]
  (let [next-pos (fn [i]
                   (if-right direction
                     [(+ x i) y]
                     [x (+ y i)]))]
    (map next-pos (range (or width (count word))))))

(defn find-positions
  [board-letters]
  (let [first-word? (not-any? some? board-letters)]
    (for [x (if first-word? [5 0] (range 0 11))
          y (if first-word? [0 5] (range 0 11))
          [direction current next] [[:right
                                     (fn [[x y]] x)
                                     (fn [[x y] i] [(+ i x) y])]
                                    [:bottom
                                     (fn [[x y]] y)
                                     (fn [[x y] i] [x (+ i y)])]]
          w (range 2 (- (inc 11)
                        (current [x y])))

          :let [tiles (tiles-taken {:x x :y y :direction direction :width w})
                blanks (into #{}
                             (filter
                               (fn [[a b]] (nil? (nth board-letters (+ a (* b 11)))))
                               tiles))
                nblanks (count blanks)
                first-tile (first tiles)
                last-tile (last tiles)]
          
          ;; Does have a blank tile
          :when (pos? nblanks)

          ;; Preceding tile is not taken
          :when (or (= (current first-tile) 0)
                    (let [[x y] (next first-tile -1)]
                      (nil? (nth board-letters
                                 (+ x (* 11 y))))))

          ;; Next tile is not taken
          :when (or (= (current last-tile) 10)
                    (let [[x y] (next last-tile 1)]
                      (nil? (nth board-letters
                                 (+ x (* 11 y))))))

          ;; First word hits middle
          :when (or (not first-word?)
                    (not-empty (filter #{[5 5]} tiles)))

          ;; At least one tile has a neighbour
          :when (or first-word?
                    (some
                      (fn [[a b]]
                        (or
                          (and (< a 10)
                               (some? (nth board-letters (+ (inc a) (* b 11)))))
                          (and (< b 10)
                               (some? (nth board-letters (+ a (* (inc b) 11)))))
                          (and (> a 0)
                               (some? (nth board-letters (+ (dec a) (* b 11)))))
                          (and (> b 0)
                               (some? (nth board-letters (+ a (* (dec b) 11)))))))
                      blanks))]

      {:x x :y y :direction direction :width w :nblanks nblanks})))

(defn build-tiles-letter-mapping
  [board]
  (reduce
    (fn [acc [[x y] letter]]
      (assoc acc (+ x (* 11 y)) letter))
    (into [] (repeat (* 11 11) nil))
    (mapcat (fn [{:keys [x y direction word] :as wordstate}]
              (map vector (tiles-taken wordstate) word))
            board)))

(def empty-freq-list
  (reduce
    (fn [m k] (assoc m k 0))
    {}
    (map char (range 97 123))))

(defn full-freq
  [w]
  (merge-with + empty-freq-list (frequencies w)))

(def valid-words
  (filter #(<= (count %) 11)
          (with-open [rdr (clojure.java.io/reader "resources/words")]
            (map clojure.string/lower-case (into [] (line-seq rdr))))))

(def word-dic
  (build-dic valid-words))

(def valid-words-set
  (into #{} valid-words))

(defn read-letter
  [board-letters [x y]]
  (nth board-letters
       (+ x (* 11 y))))

(defn read-freq-from-board
  [board-letters wordstate]
  (->> (tiles-taken wordstate)
       (map (partial read-letter board-letters))
       (apply str)
       (full-freq)))

(defn diff-board
  [a b]
  (into #{}
        (for [x (range 11)
              y (range 11)
              :let [i (+ x (* 11 y))]
              :when (and (nil? (nth b i))
                         (some? (nth a i)))]
          [x y])))

(defn freq-to-str
  [freq]
  (reduce-kv (fn [m k v] (concat m (repeat v k))) '() freq))

(defn rotate-board
  [board]
  (map (fn [{:keys [x y direction] :as wordstate}]
         (merge wordstate
                {:x y :y x :direction (opposite-direction direction)}))
       board))

(defn other-words-valid?
  [{:keys [board-letters
           available-letters-freq
           positions
           board
           one-blank-h-positions
           one-blank-v-positions]}
   {:keys [x y direction word used-letters-freq] :as wordstate}]
  (let [new-board-letters (build-tiles-letter-mapping (cons wordstate board))
        board-diff (diff-board new-board-letters board-letters)
        intersections (->> (if-right direction one-blank-v-positions one-blank-h-positions)
                           (map (fn [position]
                                  [position
                                   (set/intersection (into #{} (tiles-taken position)) board-diff)]))
                           (filter (comp not-empty second)))
        other-words (->> intersections
                         (map (fn [[position intersection]]
                                (first
                                  (find-all-possible-words
                                    board
                                    board-letters
                                    (full-freq (str (read-letter new-board-letters (first intersection))))
                                    (list position)))))
                         (filter not-empty)
                         (map first))]

    (if (= (count intersections) (count other-words))
      (assoc wordstate :other-words other-words)
      nil)))

(defn find-all-possible-words
  [board board-letters available-letters-freq positions]
  (let [one-blank-positions (->> positions
                                 (filter (fn [{:keys [nblanks]}] (= nblanks 1)))
                                 (group-by :direction))
        state {:board board
               :board-letters board-letters
               :available-letters-freq available-letters-freq
               :one-blank-h-positions (:right one-blank-positions)
               :one-blank-v-positions (:bottom one-blank-positions)}]

    (->> positions
         (mapcat (fn [position]
                   (->> (tiles-taken position)
                        (map (partial read-letter board-letters))
                        (lookup word-dic available-letters-freq)
                        (map #(merge {:word % :word-freq (full-freq %)} position)))))
         (keep (partial other-words-valid? state))
         (map (fn [{:keys [other-words] :as wordstate}]
                (cons (dissoc wordstate :other-words) other-words))))))

(defn get-score
  [available-letters-freq board-letters [x y direction word letters intersections]]
  (let [tiles (tiles-taken [x y direction word])
        scored (map (comp nil? (partial read-letter board-letters)) tiles)
        values (map (partial get letter-values) word)
        bonuses (map (fn [[s b]] (if s (get bonuses b) nil)) (map vector scored tiles))
        w2 (not-empty (filter #{:w2} bonuses))
        w3 (not-empty (filter #{:w3} bonuses))]

    (->> (map vector tiles scored word values bonuses)
         (reduce
           (fn [{:keys [score available-letters] :as m}
               [[x y] scored l value bonus]]

             (let [joker (and scored (zero? (get available-letters l 1)))]
               
               {:score (+ score (* (if joker 0 value)
                                   (if scored (get {:l2 2 :l3 3} bonus 1) 1)))

                :available-letters (if (and scored (not joker))
                                     (update available-letters l - 1)
                                     available-letters)}))
           
           {:score 0
            :available-letters available-letters-freq})
         
         (:score)
         (* (if w2 2 (if w3 3 1)))
         (+ (reduce + (map (partial
                             get-score
                             available-letters-freq
                             board-letters)
                           intersections))))))

(defn find-solutions
  [board letters]
  (let [board-letters (build-tiles-letter-mapping board)
        available-letters-freq (full-freq letters)

        positions (find-positions board-letters)

        solutions (find-all-possible-words
                    board
                    board-letters
                    available-letters-freq
                    positions)

        ;; scores (map (fn [s] [(get-score available-letters-freq board-letters s) s]) solutions)
        ]

    solutions))

(defn show-solution
  [solution]
  (map (fn [{:keys [x y direction word]}] [x y direction word])
       solution))

(defn play [nplayers]
  ((fn [{:keys [board draw turn available-letters] :as state}]
     (let [player (mod turn nplayers)
           player-draw (get draw player)
           player-new-draw (take (- 7 (count player-draw)) available-letters)

           remaining-letters
           (->> (merge-with -
                            (full-freq available-letters)
                            (full-freq player-new-draw))
                (freq-to-str)
                (shuffle))
           
           solution
           (->> (concat player-draw player-new-draw)
                (apply str)
                (find-solutions board)
                (first))]

       (if-let [[{:keys [x y direction word used-letters-freq]}] solution]
         (do
           (when debug-on
             (let [available-letters-after-play
                   (assoc draw player
                          (freq-to-str
                            (merge-with -
                                        (full-freq (concat player-new-draw player-draw))
                                        used-letters-freq)))]

               (do
                 (println "player" (inc player) "has played the word")
                 (pprint (show-solution solution))
                 (println "player letters: ")
                 (pprint (get player available-letters-after-play))
                 (println "remaining letters:" (count remaining-letters)))
               (recur {:board (concat solution board)
                       :turn (inc turn)
                       :draw available-letters-after-play
                       :available-letters remaining-letters}))))
         (do
           (when debug-on
             (println "player" (inc player) "cannot play any word, end of game"))
           state))))

   {:board '()
    :turn 0
    :draw (into [] (repeat nplayers ""))
    :available-letters (shuffle (freq-to-str available-letters))}))

(def cross-board '({:x 0 :y 0 :word "annee" :direction :right}
                   {:x 1 :y 0 :word "nez" :direction :bottom}))
