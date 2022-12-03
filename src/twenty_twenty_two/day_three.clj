(ns twenty-twenty-two.day-three
  (:require [common :as c]
            [clojure.string :as s]
            [clojure.set :as set]))


(def input (slurp "src/twenty_twenty_two/day-three-input.txt"))


(defn priority [c]
  (let [code (int c)]
    (if (< code 96)
      (- code 38)
      (- code 96))))


(defn split [s]
  (->> s
       (split-at (/ (count s) 2))
       (map set)))

;; Part 1

(->> input
     s/split-lines
     (map split)
     (map #(apply set/intersection %))
     (map #(map priority %))
     flatten
     (apply +))

;; Part 2

(->> input
     s/split-lines
     (partition 3)
     (map #(map set %))
     (map #(apply set/intersection %))
     (map (comp priority first))
     (apply +))
