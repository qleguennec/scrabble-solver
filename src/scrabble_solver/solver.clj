(ns scrabble-solver.solver
  (:require [clojure.set :as set]
            [clojure.core.reducers :as r]
            [clojure.string :as s]
            [clojure.math.combinatorics :as combo])
  (:use [scrabble-solver.tree-dic]))

(def letter-values
  {\a 1 \b 3 \c 3 \d 2 \e 1 \f 5 \g 2 \h 5 \i 1 \j 8 \k 10 \l 2 \m 2 \n 1 \o 1 \p 3 \q 10 \r 1 \s 1 \t 1 \u 2 \v 8 \w 10 \x 10 \y 10 \z 10})

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

(defn get-score
  [available-letters-freq board-letters [{:keys [word] :as solution} & solutions]]
  (if (nil? solution)
    0
    (+ (get-score available-letters board-letters solutions)
       (let [tiles (tiles-taken solution)
             scored (map (comp nil? (partial read-letter board-letters)) tiles)
             values (map (partial get letter-values) word)
             bonuses (map (fn [[s b]] (if s (get bonuses b) nil)) (map vector scored tiles))
             w2 (not-empty (filter #{:w2} bonuses))
             w3 (not-empty (filter #{:w3} bonuses))]

         (* (if w2 2 (if w3 3 1))
            (if (= 7 (count (filter identity scored)))
              50
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
                   
                   (:score))))))))

(declare find-all-possible-words)

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
               :one-blank-h-positions (:right one-blank-positions)
               :one-blank-v-positions (:bottom one-blank-positions)}

        njokers (get available-letters-freq \? 0)

        freq-combinations
        (->> (range (inc njokers))
             (map (partial combo/combinations (map #(vector (char (+ 97 %)) 1) (range 26))))
             (mapcat (partial map (partial into {})))
             (map (partial merge-with + (dissoc available-letters-freq \?))))]

    (->> freq-combinations
         (mapcat (fn [freq]
                   (->> positions
                        (mapcat (fn [position]
                                  (->> (tiles-taken position)
                                       (map (partial read-letter board-letters))
                                       (lookup word-dic freq)
                                       (map #(merge {:word % :word-freq (full-freq %)} position)))))
                        (keep (partial other-words-valid? (assoc state :available-letters freq)))
                        (map (fn [{:keys [other-words] :as wordstate}]
                               (assoc wordstate :score
                                      (get-score available-letters-freq board-letters
                                                 (cons wordstate other-words)))))
                        (map #(assoc % :board-letters (->> (tiles-taken %)
                                                           (map (partial read-letter board-letters))
                                                           (apply str)
                                                           (full-freq))))
                        (map #(assoc % :letters-used (->> (:board-letters %)
                                                          (merge-with - (:word-freq %))
                                                          (freq-to-str)
                                                          (apply str))))
                        (map #(select-keys % [:score :x :y :direction :word :letters-used]))))))))

(defn find-solutions
  [board letters n]
  (let [board-letters (build-tiles-letter-mapping board)
        available-letters-freq (full-freq letters)

        positions (find-positions board-letters)

        solutions (find-all-possible-words
                    board
                    board-letters
                    available-letters-freq
                    positions)]

    (take n (sort-by :score > solutions))))

(defn play [nplayers]
  ((fn [{:keys [board draw turn available-letters] :as state}]
     (let [player (mod turn nplayers)
           player-draw (get draw player)
           player-new-draw (apply str (concat player-draw
                                              (take (- 7 (count player-draw))
                                                    available-letters)))]
       
       (if-let [{:keys [x y direction word letters-used] :as solution}
                (first (find-solutions board player-new-draw 1))]

         (do (println "player" (inc player) "draw")
             (pprint player-new-draw)
             (println "player" (inc player) "has played the word")
             (pprint solution)

             (recur {:board (cons solution board)
                     :turn (inc turn)
                     :draw (update draw player #(freq-to-str (merge-with - (full-freq letters-used) (full-freq %))))
                     :available-letters (drop (- 7 (count player-draw)) available-letters)}))

         (do (println "player" (inc player) "cannot play any word, end of game")
             state))))

   {:board '()
    :turn 0
    :draw (into [] (repeat nplayers ""))
    :available-letters (shuffle (freq-to-str available-letters))}))
