(ns scrabble-solver.solver
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.math.combinatorics :as combo])
  (:use [scrabble-solver.util]
        [scrabble-solver.tree-dic]
        [scrabble-solver.params]))

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

         (+ (if (= 7 (count (filter identity scored)))
              35 0)
            (* (if w2 2 (if w3 3 1))
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

(declare find-solutions)

(defn with-potential-next-score
  [board available-letters-freq depth solutions]
  (if (pos? depth)
    solutions
    (->> solutions
         (map (fn [{:keys [letters-used] :as sol}]
                (let [board (cons sol board)
                      letters-used-freq (full-freq letters-used)
                      njokers (reduce-kv (fn [s k v] (if (pos? v) (+ s v) s))
                                         0
                                         (merge-with - letters-used-freq available-letters-freq))
                      available-letters-freq (merge-with - available-letters-freq {\? njokers})
                      letters (freq-to-str (merge-with - available-letters-freq letters-used-freq))]
                  (-> sol
                      (assoc :potential-next-word
                             (first (find-solutions board letters (inc depth))))
                      (#(assoc % :combined-score (+ (:score %)
                                                    (or (some-> % :potential-next-word :score) 0)))))))))))

(defn find-solutions
  ([board letters] (find-solutions board letters 0))
  ([board letters depth]
   (let [board-letters (build-tiles-letter-mapping board)
         available-letters-freq (full-freq letters)

         positions (find-positions board-letters)

         solutions (find-all-possible-words
                     board
                     board-letters
                     available-letters-freq
                     positions)]

     (->> solutions
          (sort-by :score >)
          (with-potential-next-score board available-letters-freq depth)))))
