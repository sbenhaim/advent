(ns twenty-twenty-two.day-five
  (:require [common :as c]
            [clojure.string :as s]))


(def input (slurp "src/twenty_twenty_two/day-five-input.txt"))


(def instrs (-> input (s/split #"\n\n") second s/split-lines))


(def stacks
  ["MFCWTDLB"
   "LBN"
   "VLTHCJ"
   "WJPS"
   "RLTFCSZ"
   "ZNHBGDW"
   "NCGVPSMF"
   "ZCVFJRQW"
   "HLMPR"])


(defn move [from to stacks]
  (let [[i & rs] (stacks from)
        stacks (assoc stacks from rs)
        stacks (update stacks to #(cons i %))]
    stacks))


(defn move-n [n from to stacks]
  (nth (iterate (partial move from to) stacks) n))


(defn parse-line [l]
  (let [ds (re-seq #"\d+" l)]
    (mapv c/parse-int ds)))

;; Part 1

(let [result (reduce (fn [stacks [n from to]]
                (move-n n (dec from) (dec to) stacks))
              stacks
              (map parse-line instrs))]
  (apply str (map first result)))

;; Part 2

(defn move-n2 [n from to stacks]
  (let [[ms & [rs]] (split-at n (stacks from))
        stacks (assoc stacks from rs)
        stacks (update stacks to #(concat ms %))]
    stacks))


(let [result (reduce (fn [stacks [n from to]]
                       (move-n2 n (dec from) (dec to) stacks))
                     stacks
                     (map parse-line instrs))]
  (apply str (map first result)))
