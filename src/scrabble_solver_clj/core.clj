(ns scrabble-solver-clj.core
  (:use [clojure.set :as set]
        [clojure.core.reducers :as reducers]))

(def letter-values
  {:a 1 :b 3 :c 3 :e 1 :f 4 :g 2 :h 4 :i 1 :j 8 :k 10 :l 1 :m 2 :n 1 :o 1 :p 3 :q 8 :r 1 :s 1 :t 1 :u 1 :v 4 :w 10 :x 10 :y 10 :z 10})

(def bonuses
  (hash-map
    [0 0] :l3 [2 0] :w3 [8 0] :w2 [10 0] :l3 [1 1] :w2 [5 1] :w2 [9 1] :w2 [0 2] :w3 [2 2] :l3 [4 2] :l3 [6 2] :l2 [8 2] :l3 [10 2] :w3 [3 3] :l3 [7 3] :l3 [2 4] :l2 [8 4] :l2 [1 5] :w2 [9 5] :w2 [2 6] :l2 [8 6] :l2 [3 7] :l3 [7 7] :l3 [0 8] :w3 [2 8] :l3 [6 8] :l2 [8 8] :l3 [10 8] :w3 [1 9] :w2 [5 9] :w2 [9 9] :w2 [0 10] :l3 [2 10] :w3 [8 10] :w3 [10 10] :l3))

(def cross-board '([0 0 :right "annee"]
                   [1 0 :bottom "nez"]))

(def albot '([1 5 :right "osiez"]))

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
  (group-by #(nth % 2)
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

                  ;; At least one tile has a neighbour
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

              [x y direction w])))

(defn wordspec-to-integers
  [[x y direction word-or-width]]
  [x y (if-right direction 0 1)
   (if (string? word-or-width)
     (count word-or-width)
     word-or-width)])

(defn find-crosses
  [positions positions-into]
  (apply hash-map
         (mapcat
           (fn [wordspec]
             [(wordspec-to-integers wordspec)
              (let [tiles (into #{} (tiles-taken wordspec))]
                (->> positions-into
                     (map
                       (fn [wordspec]
                         (->> wordspec
                              (tiles-taken)
                              (into #{})
                              (set/intersection tiles)
                              (vector wordspec))))
                     (filter (fn [[_ ts]] (not-empty ts)))))])
           positions)))

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

(defn read-letter
  [board-letters [x y]]
  (nth board-letters
       (+ x (* 11 y))))

(defn read-letters-from-board
  [board-letters wordspec]
  (map (partial read-letter board-letters)
       (tiles-taken wordspec)))

(defn read-freq-from-board
  [board-letters wordspec]
  (get-letter-frequency 
    (apply str
           (filter some? (read-letters-from-board board-letters wordspec)))))

(defn diff-board
  [a b]
  (into #{}
        (for [x (range 11)
              y (range 11)
              :let [i (+ x (* 11 y))]
              :when (and (nil? (nth b i))
                         (some? (nth a i)))]
          [x y])))

(defn rotate-board
  [board]
  (map (fn [[x y direction word]]
         [y x (opposite-direction direction) word]) board))

(defn dictionary-transducer
  [board board-letters available-letters-freq [x y direction width :as wordspec]]
  (comp 
    (filter
      (fn [[word target-word-freq]]
        (and
          ;; Is strictly equal to the slot width
          (= (count word) width)

          ;; Does contain all the non blank slots
          (every?
            (fn [[pos letter]]
              (let [board-letter (read-letter board-letters pos)]
                (and
                  (or (nil? board-letter)
                      (= letter board-letter)))))
            (map vector (tiles-taken wordspec) word)))))
    
    (map #(update % 1
                  sum-freq-lists
                  (scale-freq (read-freq-from-board board-letters wordspec) -1)))
    
    (filter (fn [[word needed-letters-freq]]
              ;; No letters are missing
              (let [njokers (:? available-letters-freq 0)
                    freq-diff (sum-freq-lists needed-letters-freq
                                              (scale-freq (dissoc available-letters-freq :?)
                                                          -1))
                    nmissing (get-positive-freq-sum freq-diff)] 
                (<= nmissing njokers))))

    (map (fn [[word freq]]
           [word
            (let [new-board-letters (build-tiles-letter-mapping
                                      (cons [x y direction word] board))]
              (apply str
                     (map (partial read-letter new-board-letters)
                          (diff-board new-board-letters
                                      board-letters))))]))))

(defn find-all-possible-words
  [board board-letters available-letters-freq positions]
  (->> positions
       (pmap
         (fn [[x y direction width :as wordspec]]
           (some->> words-freq-list
                    (eduction (dictionary-transducer
                                board
                                board-letters
                                available-letters-freq
                                wordspec))
                    (#(when (not-empty %) %))
                    (conj [x y direction]))))

       (mapcat (fn [[x y direction words]] (map (partial conj [x y direction]) words)))
       (group-by #(nth % 2))))

(defn find-word-crosses
  [board board-letters solutions crosses]
  (->> solutions
       (map #(update
               % 
               3
               (partial map
                        (fn [[word freq]]
                          (let [new-board (build-tiles-letter-mapping
                                            (cons [x y direction word] board))

                                new-letters (diff-board new-board board-letters)

                                crosses (get crosses
                                             (wordspec-to-integers [x y direction word])
                                             '())

                                intersections
                                (->> crosses
                                     (filter (comp
                                               not-empty
                                               (partial set/intersection new-letters)
                                               second))

                                     (filter (fn [[[x y direction word]]]
                                               (= 1
                                                  (count
                                                    (filter #(nil? (nth board-letters
                                                                        (+ (first %)
                                                                           (* 11 (second %)))))
                                                            (tiles-taken [x y direction width]))))))

                                     (mapcat (fn [[[x y direction width] join-tiles]]
                                               (->> join-tiles
                                                    (into '())
                                                    (map (fn [tile]
                                                           (->> (+ (first tile)
                                                                   (* 11 (second tile)))
                                                                (nth new-board)
                                                                (conj [x y (if-right direction 0 1)]))))))))]
                            
                            [word freq intersections])))))))

(defn build-1l-word-set
  [solutions]
  (->> solutions
       (filter (fn [[x y direction [word letters]]]
                 (= 1 (count letters))))

       (keep (fn [[word freq]]
               (hash-map
                 [x y
                  (if-right direction 0 1)
                  (some->> (get (set/map-invert freq) 1)
                           (name)
                           (first))]
                 [x y direction [word freq]])))
       (apply merge)))

(defn filter-invalid-words [words l1-words]
  (->> words
       (map #(update % 4
                     (partial sequence
                              (comp
                                (map (fn [[word crosses]]
                                       [word (map (partial get l1-words) crosses)]))
                                (filter (comp (partial not-any? nil?) second))))))
       (filter (comp not-empty #(nth % 4)))))

(defn find-solutions
  [board letters]
  (let [board-letters (build-tiles-letter-mapping board)
        available-letters-freq (get-letter-frequency letters)

        {h-positions :right v-positions :bottom} (find-positions board-letters)

        h-crosses (find-crosses h-positions v-positions)
        v-crosses (find-crosses v-positions h-positions)

        ;; {h-words :right v-words :bottom}
        words
        (find-all-possible-words
          board
          board-letters
          available-letters-freq
          (concat h-positions v-positions))
        
        ;; l1-word-set (build-1l-word-set (concat h-words v-words))
        
        ;; h-word-crosses (find-word-crosses board board-letters h-words h-crosses)
        ;; v-word-crosses (find-word-crosses board board-letters v-words v-crosses)

        ;; valid-words (filter-invalid-words (concat h-word-crosses v-word-crosses)
        ;;                                   l1-word-set)
        ]

    words))

(def result (find-solutions cross-board "s"))
