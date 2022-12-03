(ns twenty-fifteen.day1)

(def input (.trim (slurp "src/twenty_fifteen/day1-input.txt")))

;; Part 1
(reduce (fn [s i] (({\( inc \) dec} i) s)) 0 input)

;; Part 2


(loop [level 0 n 0]
  (let [c (nth input n)
        next-level (({\( inc \) dec} c) level)]
    (if (= next-level -1) (inc n)
        (recur next-level (inc n)))))
