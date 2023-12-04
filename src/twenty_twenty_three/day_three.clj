(ns twenty-twenty-three.day-three
  (:require [clojure.string :as s]
            [common :refer :all]
            [clojure.set :refer [difference]]))

(def sample "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")


(defn parse-map [s]
  (let [lines (s/split-lines s)]
    (loop [row 0 col 0 syms {} digits []]
      (if (= col (count (first lines)))
        (recur (inc row) 0 syms digits)
        (if (= row (count lines))
          [syms digits]
          (let [l     (nth lines row)
                s     (subs l col)
                sym   (re-find #"^[^\d.]" s)
                digit (re-find #"^\d+" s)
                empty (re-find #"^\." s)]
            (cond
              sym   (recur row (inc col) (assoc syms [row col] sym) digits)
              empty (recur row (inc col) syms digits)
              digit (recur row (+ col (count digit)) syms
                           (conj digits [(parse-int digit) [row col] (count digit)])))))))))


(comment (parse-map (slurp "data/day-three.txt")))


(defn adj-coords [y x len]
  (let [coords (set (for [xs (range x (+ x len))] [y xs]))
        adj    (for [xa    (range (dec x) (+ 1 x len))
                     ya    (range (dec y) (+ 2 y))
                     :when (and (>= xa 0) (>= ya 0))]
                 [ya xa])]
    (difference (set adj) coords)))


(comment
  (adj-coords 1 1 3))


(defn sym-adj? [y x len syms]
  (some #(get syms %) (adj-coords y x len)))


(comment
  ;;;  Part 1
  (let [[syms digits] (parse-map (slurp "data/day-three.txt"))
        ds (keep (fn [[d [y x] l]] (when (sym-adj? y x l syms) d)) digits)]
    (reduce + ds)))

;;; Part 2

(defn adj-gears [syms [d [y x] len]]
  (keep #(let [sym (get syms %)] (when (= sym "*") [% d])) (adj-coords y x len)))


(comment
  (let [[syms digits] (parse-map (slurp "data/day-three.txt"))
        stuff         (map (partial adj-gears syms) digits)
        stuff         (partition 3 (flatten stuff))
        gears         (into {} (reduce (fn [grs [y x d]] (update grs [y x] conj d)) {} stuff))
        ratios        (for [[_ v] gears :when (= (count v) 2)]
                        (apply * v))]
    (apply + ratios)))
