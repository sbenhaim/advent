(ns twenty-twenty-two.day-two
  (:require [clojure.string :as s]))

(def input (slurp "src/twenty_twenty_two/day-two-input.txt"))

(def play-score
  {"X" 1
   "Y" 2
   "Z" 3})


(defn points [[t m]]
  (+ (play-score m)
     (cond
       (#{["A" "X"] ["B" "Y"] ["C" "Z"]} [t m]) 3
       (#{["A" "Y"] ["B" "Z"] ["C" "X"]} [t m]) 6
       :else 0)))

;; Part 1

(->> input
     s/split-lines
     (map #(s/split % #" "))
     (map points)
     (apply +))

;; Part 2

(defn play [[t m]]
  (case [t m]
    ["A" "X"] ["A" "Z"]
    ["A" "Y"] ["A" "X"]
    ["A" "Z"] ["A" "Y"]

    ["B" "X"] ["B" "X"]
    ["B" "Y"] ["B" "Y"]
    ["B" "Z"] ["B" "Z"]

    ["C" "X"] ["C" "Y"]
    ["C" "Y"] ["C" "Z"]
    ["C" "Z"] ["C" "X"]))

(->> input
     s/split-lines
     (map #(s/split % #" "))
     (map (comp points play))
     (apply +))
