(ns twenty-twenty-three.day-two
  (:require [common :refer :all]
            [clojure.string :as s]
            [portal-dev.core :refer :all]))


(defn parse-hand
  [hand]
  (into {}
        (for [h (s/split hand #", ")]
          (let [[digit color] (s/split h #" ")]
            [(keyword color) (parse-int digit)]))))


(comment
  (parse-hand "1 red, 2 blue, 3 green"))


(defn parse-line
  [s]
  (let [[game results] (s/split s #": ")
        [_ game] (s/split game #" ")
        game (parse-int game)

        results (s/split results #"; ")
        results (map parse-hand results)]
    [game results]))


;; via thread-last
(->>
  (file->lines "data/day-two.txt")
  (map parse-line)
  (map (fn [[game results]]
         (if (and
               (every? #(<= % 12)
                       (map #(get % :red 0) results))
               (every? #(<= % 13)
                       (map #(get % :green 0) results))
               (every? #(<= % 14)
                       (map #(get % :blue 0) results)))
           game
           0)))
  (reduce +))


;; via tranduction
(transduce
 (comp (map parse-line)
       (map (fn [[game results]]
              (for [c [:red :green :blue]]
                (apply max (keep c results)))))
       (map (partial apply *)))
 +
 (file->lines "data/day-two.txt"))
