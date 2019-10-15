(ns scrabble-solver-clj.core)

(def test-board '([0 0 :right "alphabet"]
                  [0 2 :bottom "piano"]))

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
  [[x y direction word-or-width]]
  (let [width (if (string? word-or-width)
                (count word-or-width)
                word-or-width)
        next-pos (fn [i]
                   (if-right direction
                     [(+ x i) y]
                     [x (+ y i)]))]
    (map next-pos (range width))))

(defn tile-taken?
  [board [x y]]
  (some (fn [[a b]] (and (= a x)
                        (= b y)))
        (apply concat (map tiles-taken board))))

(defn get-letter-frequency
  [word]
  (frequencies (map (comp keyword str) word)))

(defn find-positions
  [board [x y direction]]
  (let [dir (if-right direction x y)
        opp (if-right direction y x)]
    (for [start (range 0 12)
          width (range 2 12)
          :let [end (+ start width)]
          :when (< end 12)
          ;; Does not have a taken tile in left/up direction
          :when (or (not (> start 0))
                    (not (tile-taken? board
                                      (if-right direction
                                        [start y direction width]
                                        [x start direction width]))))
          ;; Does not have a taken tile in right/bottom direction
          :when (or (not (< end 11))
                    (not (tile-taken? board
                                      (if-right direction
                                        [end y direction width]
                                        [x end direction width]))))
          :let [tile (if-right direction
                       [start y direction width]
                       [x start direction width])]]
      tile)))

(defn build-tiles-letter-mapping
  [board]
  (reduce
    (fn [acc [[x y] letter]]
      (assoc acc (+ x (* 11 y)) letter))
    (into [] (repeat (* 11 11) nil))
    (mapcat
      (fn [[x y direction word :as wordspec]]
        (map
          (fn [letter tiles] [tiles letter])
          word
          (tiles-taken wordspec)))
      board)))

(def empty-freq-list
  (zipmap
    (map (comp keyword str char) (range 97 123))
    (repeat 0)))

(def valid-words
  (with-open [rdr (clojure.java.io/reader "resources/words")]
    (map clojure.string/lower-case (into [] (line-seq rdr)))))

(def letter-frequency-list
  (map get-letter-frequency valid-words))

(def words-freq-list
  (map vector valid-words letter-frequency-list))

(defn scale-freq
  [freq factor]
  (reduce-kv
    (fn [m k v] (update m k * factor)) freq freq))

(defn get-positive-freq-sum
  [freq-list]
  (reduce-kv (fn [m k v] (if (> v 0)
                          (+ m v)
                          m))
             0
             freq-list))

(defn sum-freq-lists
  [& freq-lists]
  (reduce
    (fn [acc k] (assoc acc
                      k
                      (apply +
                             (map #(k % 0) freq-lists))))
    {}
    (map (comp keyword str char) (range 97 123))))

(defn read-letters-from-board
  [board-letters wordspec]
  (map (fn [[x y]] (nth board-letters
                       (+ x (* 11 y))))
       (tiles-taken wordspec)))

(defn read-freq-from-board
  [board-letters wordspec]
  (get-letter-frequency 
    (apply str
           (filter some? (read-letters-from-board board-letters wordspec)))))

(defn diff-board
  [a b]
  (for [x (range 11)
        y (range 11)
        :let [i (+ x (* 11 y))]
        :when (and (nil? (nth a i))
                   (some? (nth b i)))]
    [a b]))

(defn rotate-board
  [board]
  (map (fn [x y direction word]
         [y x (opposite-direction direction word)]) board))

(defn get-extended-board-words
  [board direction]
  (mapcat (fn [[x y dir word :as wordspec]]
            (if (= dir direction)
              `(~wordspec)
              (map (fn [[a b] letter] [a b direction (str letter)])
                   (tiles-taken wordspec) word)))
          board))

(defn find-all-possible-words
  [board-letters available-letters-freq positions]
  (map (fn [[x y direction width :as wordspec]]
         [x y direction width
          (filter
            (fn [[word target-word-freq]]
              (and
                ;; Is strictly equal to the slot width
                (= (count word) width)

                (let [n-non-blanks (count (filter some?
                                                  (read-letters-from-board board-letters wordspec)))]
                  ;; Does contain (but not only) blank slots
                  (and
                    (not (= width n-non-blanks))
                    (not (= 0 n-non-blanks))))

                ;; Does contain all the non blank slots
                (every?
                  (fn [[[a b] letter]]
                    (let [board-letter (nth board-letters
                                            (+ a (* 11 b)))]
                      (and
                        (or (nil? board-letter)
                            (= letter board-letter)))))
                  (map vector (tiles-taken wordspec) word))

                ;; No letters are missing
                (let* [njokers (:? available-letters-freq 0)
                       total-available-letters-freq
                       (sum-freq-lists (dissoc available-letters-freq :?)
                                       (read-freq-from-board board-letters wordspec))
                       freq-diff (sum-freq-lists target-word-freq
                                                 (scale-freq total-available-letters-freq -1))
                       nmissing (get-positive-freq-sum freq-diff)] 
                  (<= nmissing njokers))))

            words-freq-list)])
       positions))

(defn find-solutions
  [board letters]
  (let* [board-letters (build-tiles-letter-mapping board)
         available-letters-freq (get-letter-frequency letters)
         horiz-positions (mapcat (fn [[x y]] (find-positions board [x y :right]))
                                 (get-extended-board-words board :right))
         vert-positions (mapcat (fn [[x y]] (find-positions board [x y :bottom]))
                                (get-extended-board-words board :bottom))

         horiz-words
         (filter (fn [[x y direction width words]] (not-empty words))
                 (find-all-possible-words
                   board-letters
                   available-letters-freq
                   horiz-positions))

         vert-words
         (filter (fn [[x y direction width words]] (not-empty words))
                 (find-all-possible-words
                   board-letters
                   available-letters-freq
                   vert-positions))]

    (doseq [h (mapcat (fn [[x y dir width words]]
                        (map (fn [[a b]] a) words))
                      horiz-words)]
      (pprint ['H h]))
    (doseq [v (mapcat (fn [[x y dir width words]]
                        (map (fn [[a b]] a) words))
                      vert-words)]
      (pprint ['V v]))))
