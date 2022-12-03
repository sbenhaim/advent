(ns twenty-twenty-two.day-one
  (:require [common :as c]
            [clojure.string :as s]))


(def input "src/twenty_twenty_two/day-one-input.txt")


(defn split-n-parse [s]
  (->> s s/split-lines (map c/parse-int)))


(def amts
  (->> (s/split (slurp input) #"\n\n")
       (map split-n-parse)
       (map #(apply + %))))


;; Part 1
(apply max amts)


;; Part 1
(apply + (take 3 (reverse (sort amts))))
