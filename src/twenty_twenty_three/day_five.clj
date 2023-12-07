(ns twenty-twenty-three.day-five
  (:require [clojure.string :as s]
            [common :refer [parse-int]]))

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


(comment (s->ints "1 23 34"))


(defn build-map
  [s]
  (let [[a b r] (s->ints s)]
    [(range b (+ b r)) (range a (+ a r))]))


(comment (let [m (build-map "4 3 4")]
           ))


(defn parse-map
  [s]
  (let [lines (s/split-lines s)
        map-name (re-find #"^[\w-]+" (first lines))
        maps (map build-map (rest lines))]
    maps
    #_{map-name (zipmap (map first maps) (map second maps))}))

(comment
  (parse-map "name:
4 3 4
9 9 2"))


(defn parse-almanac
  [s]
  (let [instructions (s/split s #"\n\n")
        seeds        (->> instructions first (re-seq #"\d+") (map parse-int))
        raw-maps         (map parse-map (rest instructions))
        maps (apply merge raw-maps)]
    (assoc maps :seeds seeds)))

(comment

  (parse-almanac sample)

  ;; Careful: Very large
  (parse-almanac almanac))


(defn seed->location
  [maps seed]
  (let [soil        (get (maps "seed-to-soil") seed seed)
        fertilizer  (get (maps "soil-to-fertilizer") soil soil)
        water       (get (maps "fertilizer-to-water") fertilizer fertilizer)
        light       (get (maps "water-to-light") water water)
        temperature (get (maps "light-to-temperature") light light)
        humidity    (get (maps "temperature-to-humidity") temperature temperature)
        location    (get (maps "humidity-to-location") humidity humidity)]
    location))


(comment
  (seed->location 13 (parse-almanac sample)))


(defn solve-part-1 [s]
  (let [{:keys [seeds] :as almanac} (parse-almanac s)
        locations (map (partial seed->location almanac) seeds)]
    (tap> locations)
    (apply min locations)))


(comment
  (tap> (solve-part-1 sample))
  (tap> (solve-part-1 (slurp "data/day-five.txt"))))
