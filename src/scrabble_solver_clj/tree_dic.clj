(ns scrabble-solver-clj.tree-dic
  (:require [clojure.core.reducers :as r]))

(def test-words '("aa" "abc" "no"))

(def empty-dic
  (into []
        (repeat 12
                ['()
                 (into [] (repeat 26 []))])))

(defn insert-word
  [dic word]
  (letfn [(re [dic [l & r] i]
            (if (= (count word) i)
              (update-in (or (not-empty dic) empty-dic) [i 0] conj word)
              (update (or (not-empty dic) empty-dic) i
                      (fn [[words l-dic]]
                        [words
                         (update l-dic (- (int l) 97)
                                 #(re % r (inc i)))]))))]
    (re dic word 0)))

(defn build-dic
  [words]
  (reduce insert-word empty-dic words))

(defn lookup
  [dic freq word]
  (letfn [(re [dic freq [l & r] i]
            (if (not-empty dic)
              (let [[words l-dic] (nth dic i)]
                (if (= (count word) i)
                  words
                  (if (some? l)
                    (re (nth l-dic (- (int l) 97)) freq r (inc i))
                    (->> freq
                         (mapcat (fn [[l n]]
                                   (if (pos? n)
                                     (re (nth l-dic (- (int l) 97))
                                         (merge freq {l (dec n)})
                                         r
                                         (inc i))
                                     '())))))))
              '()))]
    (re dic freq word 0)))
