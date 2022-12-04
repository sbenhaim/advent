(ns twenty-twenty-two.day-four
  (:require [common :as c]
            [clojure.string :as s]
            [clojure.set :as set]))


(def input (slurp "src/twenty_twenty_two/day-four-input.txt"))

(defn parse-line [l]
  (let [ns (s/split l #"[,-]")
        ints (mapv c/parse-int ns)
        [a1 a2 b1 b2] ints]
    [[a1 a2] [b1 b2]]))

(def parsed
  (->> input
     s/split-lines
     (map parse-line)))

(defn subsumed? [[[a1 a2] [b1 b2]]]
  (or (and (<= b1 a1) (>= b2 a2))
      (and (<= a1 b1) (>= a2 b2))))


;; Part 1
(->> parsed
     (map subsumed?)
     (filter true?)
     count)

;; Part 2

(defn overlaps? [[[a1 a2] [b1 b2]]]
  (not (or (and (< b1 a1) (< b2 a1))
           (and (> b1 a2) (> b2 a2)))))

(->> parsed
     (map overlaps?)
     (filter true?)
     count)
