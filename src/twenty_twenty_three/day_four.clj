(ns twenty-twenty-three.day-four
  (:require [clojure.string :as s]
            [common :refer :all]
            [clojure.set :refer [intersection]]))


(def sample "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")


(defn parse-card [card]
  (let [card (-> card
                 (s/split #": ")
                 second)
        nums (re-seq #"\d+|\|" card)
        [winners _ draw] (partition-by #(= % "|") nums)]
    [(set winners) (set draw)]))

(comment (parse-card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"))


(defn score-card
  [card]
  (->> (apply intersection card)
       count
       dec
       (Math/pow 2)
       (Math/floor)))                   ; 2^-1 becomes 0, others unchanged


(defn score-collection [s]
  (transduce (comp (map parse-card)
                   (map score-card))
             +
             (s/split-lines s)))


(comment (score-collection sample))

(comment
  (tap> (score-collection (slurp "data/day-four.txt"))) ;=> 21821
  )

;; Part 2

(defn score-card-2
  [n card]
  (let [n (inc n)                       ; 1-indexed
        pts (->> (apply intersection card)
                 count)]
    [n (range (inc n) (+ n pts 1))]))


(comment (score-card-2 1 (parse-card "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")))


(defn build-card-map
  "Given a string of cards, return a map of card number to winning card numbers."
  [s]
  (let [initial (->> s
                     s/split-lines
                     (map parse-card)
                     (map-indexed score-card-2)
                     (into {}))]
    ;; Add a 0 card to represent the deck itself.
    (assoc initial 0 (range 1 (inc (count initial))))))


(comment (build-card-map sample))


(defn count-cards
  ([card-map] (dec (count-cards card-map 0)))
  ([card-map idx]
   (let [won (card-map idx)]
     (inc                                              ; +1 for the card itself
       (if (seq won)                                   ; if there are winnders
         (reduce + (map #(count-cards card-map %) won)) ; count their cards
         0)))))                                               ; else 0


;; We'll see the same cards over and over again in a single deck, so memoize the results.
;; Will improve performance but also prevent blowing the stack on large inputs.
(def count-cards (memoize count-cards))


(comment
  (let [card-map (build-card-map (slurp "data/day-four.txt"))]
    (count-cards card-map)))
