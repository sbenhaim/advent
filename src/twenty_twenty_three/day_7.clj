(ns twenty-twenty-three.day-7
  (:require [common :refer [parse-int]]
            [clojure.string :as s]))


(comment (require '[portal-dev.core] :reload))


(def sample "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")


(def input (slurp "data/day-seven.txt"))



(defn card-rank
  [card]
  (case card
    \A 14
    \K 13
    \Q 12
    \J 11
    \T 10
    (parse-int (str card))))


(comment (tap> (card-rank "9")))


(def hands [:one-pair :two-pair :three-of-a-kind :full-house :four-of-a-kind :five-of-a-kind])



(def hand-ranks (into {} (map-indexed (fn [i h] [h (+ i 1)]) hands)))

(comment
  (tap> hand-ranks))



(defn rank-hand
  [hand]
  (let [hand    (map card-rank hand)
        lead    (first hand)
        f       (frequencies hand)
        f       (for [[k v] f] [v k])
        f       (sort (comp - compare) f)
        [f & r] f
        rank    (case (first f)
                  5 (:five-of-a-kind hand-ranks)
                  4 (:four-of-a-kind hand-ranks)
                  3 (if (= 2 (ffirst r))
                      (:full-house hand-ranks)
                      (:three-of-a-kind hand-ranks))
                  2 (if (= 2 (ffirst r))
                      (:two-pair hand-ranks)
                      (:one-pair hand-ranks))
                  1 0
                  nil 0)]
    (vec (conj hand rank))))


(comment 
  (rank-hand "")
  (rank-hand "AAAAA")
  (rank-hand "AAAKA")
  (rank-hand "KKAAA")
  (rank-hand "KTAAA")
  (rank-hand "JKJAA")
  (rank-hand "JJKTA")
  (rank-hand "12545")
  (rank-hand "TJQKA"))


(defn solve-part-1
  [i]
  (let [hands-and-bids (re-seq #"(\w+) (\d+)" i)
        h-and-b        (for [[_ h b] hands-and-bids]
                         [(rank-hand h) (parse-int b)])
        h-and-b        (sort-by first h-and-b)
        winnings       (map-indexed (fn [i [h b]] [h (inc i) b (* (inc i) b)]) h-and-b)]
    (tap> (take 20 winnings))
    (reduce + (map last winnings))))


(comment
  (tap> (solve-part-1 sample))
  (tap> (solve-part-1 input)))

;; Part 2


(defn upgrade-hand-rank
  [rank]
  (case rank
          0 1 ;; From no pairs, 1 pair
          1 3 ;; From 1 pair, 3 of a kind
          2 4 ;; From 2 pair, full house
          3 5 ;; From 3 of a kind, 4 of a kind
          4 5 ;; From full house, 4 of a kind
          5 6 ;; From 4 of a kind, 5 of a kind
          6 6 ;; Cant improve on 5 of a kind
          ))


(comment (repeatedly upgrade-hand-rank 0)
         (re-seq #"J""AJKJA"))


(defn optimize-and-rank-hand
  [hand]
  (let [wild-rx #"J"
        jokers (count (re-seq wild-rx hand))
        new-hand (s/replace hand wild-rx "")]
    ;; Produce the optimal hand where jokers are wild
    (if-not jokers
      hand
      (let [[rank & _] (rank-hand new-hand)
            new-rank (nth (iterate upgrade-hand-rank rank) jokers)]
        (vec
          (conj (map card-rank hand) new-rank))))))


(comment
  (optimize-and-rank-hand "AAJJT")
  (optimize-and-rank-hand "JJAAA"))


(defn solve-part-2
  [i]
  (let [hands-and-bids (re-seq #"(\w+) (\d+)" i)
        h-and-b        (for [[_ h b] hands-and-bids]
                         [(optimize-and-rank-hand h) (rank-hand h) (parse-int b)])
        sorted-h-and-b (sort-by first h-and-b)
        winnings       (map-indexed (fn [i [h+ h b]] [h+ h (inc i) b (* (inc i) b)]) sorted-h-and-b)]
    (tap> (take 20 h-and-b))
    (reduce + (map last winnings))))


(comment (tap> (solve-part-2 sample))
         (tap> (solve-part-2 input)))
