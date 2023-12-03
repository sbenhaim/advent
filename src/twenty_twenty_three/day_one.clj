(ns twenty-twenty-three.day-one
  (:require [common :refer :all]
            [clojure.string :as s]
            [portal-dev.core :refer :all]))


(defn first-digit
  [s]
  (when-let [m (re-find #"^[^\d]*?(\d)" s)]
    (second m)))


(comment
  (re-find #"^*?(\d)" "123")
  (first-digit "asfadf1asd3afd"))


(defn last-digit
  [s]
  (when-let [m (re-find #"(\d)[^\d]*?$" s)]
    (second m)))


(comment
  (last-digit "asfadf1asd3afd"))


(defn calibration
  [s]
  (let [a (first-digit s)
        b (last-digit s)]
    (when (and a b)
      (parse-int (str a b)))))


(comment
  (calibration "asf2dfbasd3afd"))



(->> (file->lines "data/day-one.txt")
     (map calibration)
     (reduce +)
     (println))


;;; Part Two

(def tx {"1" 1
         "one" 1
         "2" 2
         "two" 2
         "3" 3
         "three" 3
         "4" 4
         "four" 4
         "5" 5
         "five" 5
         "6" 6
         "six" 6
         "7" 7
         "seven" 7
         "8" 8
         "eight" 8
         "9" 9
         "nine" 9})


(def pat (re-pattern (str "(" (s/join "|" (keys tx)) ")")))


(comment (re-seq pat "one2twobalbigwart6"))


(defn calibration
  [s]
  (let [a (first-digit s)
        b (last-digit s)]
    (when (and a b)
      (parse-int (str a b)))))
