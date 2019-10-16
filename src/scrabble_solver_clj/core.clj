(ns scrabble-solver-clj.core)

(def test-board '([0 0 :right "alphabet"]
                  [2 0 :bottom "piano"]
                  [2 2 :right "annee"]))

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
  [board-letters]
  (for [x (range 0 11)
        y (range 0 11)
        [direction current next] [[:right
                                   (fn [[x y]] x)
                                   (fn [[x y] i] [(+ i x) y])]
                                  [:bottom
                                   (fn [[x y]] y)
                                   (fn [[x y] i] [x (+ i y)])]]
        w (range 2 (- (inc 11)
                      (current [x y])))

        :let [tiles (tiles-taken [x y direction w])
              blanks (into #{} (filter
                                 (fn [[a b]]
                                   (let [letter (nth board-letters (+ a (* b 11)))]
                                     (nil? letter)))
                                 tiles))
              first-tile (first tiles)
              last-tile (last tiles)
              prev-tile (next first-tile -1)
              next-tile (next last-tile 1)]

        :when (not-empty blanks)

        :when (or (not (> (current first-tile) 0))
                  (nil? (nth board-letters
                             (+ (nth prev-tile 0)
                                (* 11 (nth prev-tile 1))))))

        :when (or (not (< (current last-tile) 10))
                  (nil? (nth board-letters
                             (+ (nth next-tile 0) 
                                (* 11 (nth next-tile 1))))))
        :when (some
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
                blanks)]
    [x y direction w]))

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
  (map (fn [[x y direction word]]
         [y x (opposite-direction direction) word]) board))

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
          (map first
               (filter
                 (fn [[word target-word-freq]]
                   (and
                     ;; Is strictly equal to the slot width
                     (= (count word) width)

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

                 words-freq-list))])
       positions))

(defn denormalize
  [words]
  (->> words
       (filter (comp not-empty #(nth % 4)))
       (mapcat (fn [[x y direction width ws]]
                 (map (fn [w] [x y direction w]) ws)))))

(defn find-crosses
  [wordspec opposite-direction-words]
  (let [tiles (into #{} (tiles-taken wordspec))]
    (filter
      (fn [[x y direction word]]
        (not-empty
          (clojure.set/intersection
            tiles
            (into #{} (tiles-taken [x y (opposite-direction direction) word])))))
      opposite-direction-words)))

(defn cross-word-sets
  [h-words v-words]
  [(map (fn [wordspec]
          {:wordspec wordspec :crosses (find-crosses wordspec v-words)})
        h-words)])

(defn find-solutions
  [board letters]
  (let* [board-letters (build-tiles-letter-mapping board)
         available-letters-freq (get-letter-frequency letters)
         words (->> (find-positions board-letters)
                    (find-all-possible-words
                      board-letters
                      available-letters-freq)
                    (denormalize))]

    words))

