(ns scrabble-solver-clj.core
  (:require [clojure.set :as set]
            [clojure.core.reducers :as r]
            [clojure.string :as s]))

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
                first-tile (first tiles)
                last-tile (last tiles)]
          
          ;; Does have a blank tile
          :when (not-empty blanks)

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

      {:x x :y y :direction direction :width w})))

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

(def valid-words-set
  (into #{} valid-words))

(def letter-frequency-list
  (map full-freq valid-words))

(def words-freq-list
  (->> (map vector valid-words letter-frequency-list)
       (shuffle)
       (into [])))

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
  (map (fn [{:keys [x y direction word]}] {:x y :y x :direction (opposite-direction direction) :word word})
       board))

(defn read-board-words [board]
  (->> board
       (build-tiles-letter-mapping)
       (replace {nil \newline})
       (apply str)
       (s/split-lines)
       (filter #(>= (count %) 2))
       (into #{})))

(defn fits-in-slot?
  [{:keys [board-letters]} {:keys [word width] :as wordstate}]
  (if (and
        ;; Is strictly equal to the slot width
        (= (count word) width)

        ;; Does contain all the non blank slots
        (->> word
             (map vector (tiles-taken wordstate))
             (every?
               (fn [[pos letter]]
                 (let [board-letter (read-letter board-letters pos)]
                   (or (nil? board-letter)
                       (= letter board-letter)))))))
    wordstate
    nil))

(defn enough-letters?
  [{:keys [board-letters available-letters-freq]}
   {:keys [word-freq] :as wordstate}]
  (let [needed-letters-freq (merge-with - word-freq (read-freq-from-board board-letters wordstate))
        njokers (get available-letters-freq \? 0)
        freq-diff (merge-with - (dissoc available-letters-freq \?) needed-letters-freq)
        nmissing (- (reduce + (filter neg? (map second freq-diff))))] 
    
    (if (>= njokers nmissing)
      (assoc wordstate :used-letters-freq needed-letters-freq)
      nil)))

(defn other-words-valid?
  [{:keys [board h-words v-words]}
   {:keys [direction] :as wordstate}]
  (let [prev-set (if-right direction v-words h-words)
        opposite-words-set (->> (cons wordstate board)
                                ((if-right direction rotate-board identity))
                                (group-by :direction)
                                ((opposite-direction direction))
                                (read-board-words))
        new-words-set (set/difference opposite-words-set prev-set)]

    (if (set/subset? new-words-set valid-words-set)
      (assoc wordstate :other-words (into '() new-words-set))
      nil)))

(defn seek-transducer
  [state [word word-freq]]
  (comp (map (fn [{:keys [x y direction] :as wordstate}]
               (some->> (merge wordstate {:word word :word-freq word-freq})
                        (fits-in-slot? state)
                        (enough-letters? state)
                        (other-words-valid? state))))
        (filter some?)))

(defn find-all-possible-words
  [board board-letters available-letters-freq positions]
  (let [{h-words :right v-words :bottom} (group-by :direction board)
        h-words (read-board-words h-words)
        v-words (read-board-words (rotate-board v-words))
        state {:h-words h-words
               :v-words b-words
               :board board
               :board-letters board-letters
               :available-letters-freq available-letters-freq}]
    
    (r/fold
      (/ (count valid-words) 1024)
      r/cat
      (fn [acc word] (transduce
                      (seek-transducer state word)
                      (fn
                        ([] {})
                        ([s] s)
                        ([s wordstate] (cons wordstate s)))
                      acc
                      positions))
      words-freq-list)))

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

(defn show-solutions
  [board letters]
  (map (fn [{:keys [x y direction word other-words]}] [x y direction word other-words])
       (find-solutions board letters)))

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

       (if-let [{:keys [x y direction word used-letters-freq]} solution]
         (do
           (when debug-on
             (do
               (println "player" (inc player) "has played the word" word "on turn" turn)
               (println "remaining letters:" (count remaining-letters))))
           (recur {:board (cons solution board)
                   :turn (inc turn)
                   :draw (assoc draw player
                                (freq-to-str
                                  (merge-with -
                                              (full-freq (concat player-new-draw player-draw))
                                              used-letters-freq)))
                   :available-letters remaining-letters}))
         (do
           (when debug-on
             (println "player" (inc player) "cannot play any word, end of game"))
           state))))

   {:board '()
    :turn 0
    :draw (into [] (repeat nplayers ""))
    :available-letters (shuffle (freq-to-str available-letters))}))

(def cross-board '({:x 0 :y 0 :word "annee"}
                   {:x 1 :y 0 :word "nez"}))
