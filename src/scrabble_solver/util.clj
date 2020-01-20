(ns scrabble-solver.util
  (:require [clojure.math.combinatorics :as combo]
            [clojure.core.reducers :as r]))

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

(defn read-letter
  [board-letters [x y]]
  (nth board-letters
       (+ x (* 11 y))))

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
  [board-letters]
  (for [x (range 11)
        y (range 11)]
    (nth board-letters (+ x (* y 11)))))

(defn sized-subsets [items min-count max-count]
  (mapcat (fn [n] (combo/combinations items n))
          (range min-count (inc max-count))))

(defn subv-ordered-partitions [coll & {from :min to :max}]
  (let [v (vec coll)
        cnt (count v)
        smin (dec (or from 1))
        smax (dec (or to cnt))]
    (map (fn [splits]
           (map (fn [start end] (subvec v start end))
                (conj splits 0)
                (concat splits (list cnt))))
         (sized-subsets (range 1 cnt) smin smax))))

(defn read-board-direction
  [board-letters direction]
  (->> board-letters
       (partition 11)
       (map #(subv-ordered-partitions % :min 2 :max 11))
       (map (partial eduction (comp
                          (filter (partial every? (fn [part] (or (every? nil? part) (every? some? part)))))
                          (map (partial map (fn [part] (if (every? nil? part) (count part) (apply str part)))))
                          (map (partial map vector (apply concat (repeat [[string? number?] [number? string?]]))))
                          (filter #(or (every? (fn [[[pred] part]] (pred part)) %)
                                       (every? (fn [[[_ pred] part]] (pred part)) %)))
                          (map (partial map second)))))
       (map first)
       (map-indexed
         (fn [y xs] (reduce (fn [[x r] w]
                             (let [width (if (number? w) w (count w))]
                               [(+ x width)
                                (if (number? w)
                                  r
                                  (conj r {:x x :y y :word w :direction direction}))]))
                           [0 '()] xs)))
       (map second)
       (flatten)
       (filter (fn [{:keys [word]}] (>= (count word) 2)))))

(defn read-board
  [board-letters]
  (concat (read-board-direction board-letters :right)
          (read-board-direction (rotate-board board-letters) :bottom)))

