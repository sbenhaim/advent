(ns twenty-fifteen.day2
  (:require [clojure.string :as s]
            [common :refer [parse-int]]))

(def input (->> (slurp "src/twenty_fifteen/day2-input.txt")
                str/split-lines
                (map #(s/split % #"x"))
                (map #(map parse-int %))))

;; Part 1

(apply +
       (for [[l w h] input]
         (let [a (* l w)
               b (* w h)
               c (* l h)
               s (min a b c)]
           (+ (* 2 (+ a b c)) s))))

;; Part 2
(apply +
       (for [[l w h] input]
         (let [[a b] (butlast (sort [l w h]))
               p (+ (* 2 a) (* 2 b))
               v (* l w h)]
           (+ p v))))
