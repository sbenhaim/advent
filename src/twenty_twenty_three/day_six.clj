(ns twenty-twenty-three.day-six 
  (:require [clojure.string :as s]))

;; Sample
;; Time:      7  15   30
;; Distance:  9  40  200

(def sample [[7 9] [15 40] [30 200]])

;; Input
;; Time:        47     84     74     67
;; Distance:   207   1394   1209   1014

(def input [[47 207] [84 1394] [74 1209] [67 1014]])

(defn distance
  [total-time hold-time]
  (* hold-time (- total-time hold-time)))


(defn distances
  [t]
  (map (juxt identity (partial distance t)) (range 1 t)))


(comment (distances 7))


(defn win-race
  [[t d]]
  (keep (fn [[t+ d+]] (when (> d+ d) [t+ d+])) (distances t)))


(comment (-> (win-race [7 9]) count))


(defn solve-part-1
  [races]
  (let [tx (comp (map win-race) (map count))]
    (transduce tx * races)))


(comment
  (solve-part-1 sample)
  (solve-part-1 input))


;; Part 2

(comment
  (solve-part-1 [[71530 940200]])
  (solve-part-1 [[47847467 207139412091014]])
  )
