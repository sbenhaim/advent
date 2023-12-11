(ns twenty-twenty-three.day-five
  (:require [clojure.string :as s]
            [common :refer [parse-int]]
            [common :as c]))


(comment (require '[portal-dev.core]))

(def sample "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")



(def almanac (slurp "data/day-five.txt"))


(defn s->ints
  [s]
  (map parse-int (re-seq #"\d+" s)))


(defn parse-range
  [s]
  (let [[a b r] (s->ints s)]
    {:target a :source b :range r :diff (- a b)}))


(comment (parse-range "1 10 12"))


(defn i->o
  [i rs]
  (loop [[{:keys [source range diff]} & rs] rs]
    (if source
      (if (<= source i (+ source range))
        (+ i diff)
        (recur rs))
      i)))


(comment (i->o 33 [(parse-range "1 10 12") (parse-range "19 20 12")]))


(defn parse-almanac
  [s]
  (let [instructions (s/split s #"\n\n")
        seeds    (->> instructions first (re-seq #"\d+") (map parse-int))
        raw-maps (->> instructions rest (map s/split-lines))
        [names raw-ranges] [(map first raw-maps) (map rest raw-maps)]
        ranges (map (partial map parse-range) raw-ranges)
        almanac (zipmap names ranges)]
    (assoc almanac :seeds seeds)))

(comment

(tap>
 (parse-almanac sample))

  ;; Careful, very large
  (tap>
   (parse-almanac almanac)))


(defn seed->location
  [maps seed]
  (-> seed
      (i->o (maps "seed-to-soil map:"))
      (i->o (maps "soil-to-fertilizer map:"))
      (i->o (maps "fertilizer-to-water map:"))
      (i->o (maps "water-to-light map:"))
      (i->o (maps "light-to-temperature map:"))
      (i->o (maps "temperature-to-humidity map:"))
      (i->o (maps "humidity-to-location map:"))))


(comment
  (let [almanac (parse-almanac sample)]
    (seed->location almanac 13)))


(defn solve-part-1 [s]
  (let [{:keys [seeds] :as almanac} (parse-almanac s)
        locations (map (partial seed->location almanac) seeds)]
    (apply min locations)))


(comment
  (solve-part-1 sample)
  (solve-part-1 (slurp "data/day-five.txt"))
  )

;; Part 2


(defn o->i
  [o rs]
  (loop [[{:keys [target range diff]} & rs] rs]
    (if target
      (if (<= target o (+ target range))
        (- o diff)
        (recur rs))
      o)))


(comment (o->i 14 [(parse-range "1 10 12")]))


(defn location->seed
  [maps location]
  (-> location
      (o->i (maps "humidity-to-location map:"))
      (o->i (maps "temperature-to-humidity map:"))
      (o->i (maps "light-to-temperature map:"))
      (o->i (maps "water-to-light map:"))
      (o->i (maps "fertilizer-to-water map:"))
      (o->i (maps "soil-to-fertilizer map:"))
      (o->i (maps "seed-to-soil map:"))))


(comment
  (seed->location (parse-almanac sample) 14)
  (location->seed (parse-almanac sample) 43))


;; Doesn't work. Too slow.

(defn get-seed-ranges
  [is]
  (let [ps (partition 2 is)]
    (for [[from r] ps]  {:from from :range r})))


(defn in-range?
  [i r]
  (<= (:from r) i (+ (:from r) (:range r))))


(defn in-ranges?
  [i rs]
  (some (partial in-range? i) rs))


(comment
  (in-ranges? 12 [{:from 1 :range 10} {:from 19 :range 10}])
  (in-ranges? 20 [{:from 1 :range 10} {:from 19 :range 10}])
  (in-ranges? 21 [{:from 1 :range 10} {:from 19 :range 10}])


(defn get-input-for-range
  [r]
  (range (:source r) (+ (:source r) (:range r))))


(defn get-output-for-range
  [r]
  (range (:target r) (+ (:target r) (:range r))))

(comment

  (def almanac+ (parse-almanac almanac))
  (def sample+ (parse-almanac sample))

(time
 (let [maps (get almanac+ "humidity-to-location map:")
       sorted (sort-by :target maps)
       r (first sorted)
       is (get-output-for-range r)
       sample (take 1e6 is)
       seeds (map (partial location->seed almanac+) sample)
       ranges (get-seed-ranges (:seeds almanac+))
       locations-and-seeds (map vector sample seeds)]
   (first
    (filter (fn [[l s]] (when (in-ranges? s ranges) [l s])) locations-and-seeds))))
  


  (+ 1 1)


  (let [ms (get alm "temperature-to-humidity map:")
        sms (sort-by :source ms)
        m (first sms)
        is (get-output-for-range m)]
    is)
  )