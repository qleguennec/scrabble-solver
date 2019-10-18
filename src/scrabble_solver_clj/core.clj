(ns scrabble-solver-clj.core)

(def test-board '([2 4 :bottom "boitant"]
                  [1 5 :right "souks"]
                  [0 10 :right "jeteras"]
                  [5 0 :bottom "pathos"]))

(def cross-board '([0 0 :right "annee"]
                   [1 0 :bottom "nez"]))

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

    [x y direction w]))

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
                              (clojure.set/intersection tiles)
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

(defn find-all-possible-words
  [board-letters available-letters-freq positions]
  (map (fn [[x y direction width :as wordspec]]
         [x y direction width
          (->> words-freq-list
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
                           (<= nmissing njokers)))))])
       positions))

(defn find-word-crosses
  [board board-letters solutions crosses]
  (->> solutions
       (map (fn [[x y direction :as words]]
              (update
                words 4
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
                                                (partial clojure.set/intersection new-letters)
                                                second))

                                      (filter (fn [[[x y direction width]]]
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
                             
                             [word intersections]))))))))

(defn build-1l-word-set
  [solutions]
  (->> solutions
       (map (fn [[x y direction width words]]
              (->> words
                   (filter (fn [[word freq]]
                             (= 1 (get-positive-freq-sum freq))))

                   (keep (fn [[word freq]]
                           (hash-map
                             [x y
                              (if-right direction 0 1)
                              (some->> (get (clojure.set/map-invert freq) 1)
                                       (name)
                                       (first))]
                             [x y direction word])))
                   (apply merge))))
       (apply merge)))

(defn filter-invalid-words [words]
  (->> (concat (:h-w-crosses result) (:v-w-crosses result))
       (map #(update % 4
                     (partial sequence
                              (comp
                                (map (fn [[word crosses]]
                                       [word (map (partial get (:l1-w result)) crosses)]))
                                (filter (comp (partial not-any? nil?) second))))))
       (filter (comp not-empty #(nth % 4)))))

(defn find-solutions
  [board letters]
  (let [board-letters (build-tiles-letter-mapping board)
        available-letters-freq (get-letter-frequency letters)
        positions (find-positions board-letters)

        {h-positions :right v-positions :bottom} (group-by #(nth % 2) positions)

        h-crosses (find-crosses h-positions v-positions)
        v-crosses (find-crosses v-positions h-positions)

        {h-words :right v-words :bottom} (->> positions
                                              (find-all-possible-words
                                                board-letters
                                                available-letters-freq)
                                              (filter #(not-empty (nth % 4)))
                                              (group-by #(nth % 2)))
        
        l1-word-set (build-1l-word-set (concat h-words v-words))
        
        h-word-crosses (find-word-crosses board board-letters h-words h-crosses)
        v-word-crosses (find-word-crosses board board-letters v-words v-crosses)

        valid-words (filter-invalid-words (concat h-words v-words))]

    {:h-words h-words
     :v-words v-words
     :h-crosses h-crosses
     :v-crosses v-crosses
     :h-w-crosses h-word-crosses
     :v-w-crosses v-word-crosses
     :l1-w l1-word-set
     :valid-words valid-words}))
