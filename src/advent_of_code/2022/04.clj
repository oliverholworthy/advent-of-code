(ns advent-of-code.2022.04
  "Day 4: Camp Cleanup"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (str/split-lines (slurp (io/resource "2022/04.txt"))))

(defn parse-ranges [line]
  (->> line
       (re-matches #"(\d+)-(\d+),(\d+)-(\d+)")
       (rest)
       (map #(Long/parseLong %))
       (partition 2)))

(defn fully-contained? [[a1 a2] [b1 b2]]
  (or (and (<= a1 b1) (>= a2 b2))
      (and (>= a1 b1) (<= a2 b2))))

(defn part-one [input]
  "Number of pairs where one fully contains the other."
  (count (filter (comp #(apply fully-contained? %) parse-ranges) input)))

(defn overlaps? [[a1 a2] [b1 b2]]
  (or (and (<= b1 a2) (>= b1 a1))
      (and (<= a1 b2) (>= a1 b1))))

(defn part-two [input]
  "Number of pairs where the range overlaps"
  (count (filter (comp #(apply overlaps? %) parse-ranges) input)))

(comment
  (part-one input)
  (part-two input)
  )
