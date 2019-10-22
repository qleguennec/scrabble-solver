(ns scrabble-solver.tree-dic
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

(def lookup-rec
  (memoize
    (fn [dic freq word [l & r] i]
      (if (not-empty dic)
        (let [[words l-dic] (nth dic i)]
          (if (= (count word) i)
            words
            (if (some? l)
              (lookup-rec (nth l-dic (- (int l) 97)) freq word r (inc i))
              (->> freq
                   (mapcat (fn [[l n]]
                             (if (pos? n)
                               (lookup-rec (nth l-dic (- (int l) 97))
                                           (merge freq {l (dec n)})
                                           word
                                           r
                                           (inc i))
                               '()))))))) '()))))

(defn lookup
  [dic freq word]
  (lookup-rec dic freq word word 0))
