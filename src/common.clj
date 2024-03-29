(ns common
  (:require [clojure.string :as s]))

(defn parse-int [s]
  (Long/parseLong s))

(defn parse-float [s]
  (Float/parseFloat s))


(defn file->lines [path]
  (-> path slurp s/split-lines))
