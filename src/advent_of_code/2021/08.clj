(ns advent-of-code.2021.08
  "Day 8: Seven Segment Search"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [s]
  (mapv (fn [line] (map (fn [patterns] (str/split (str/trim patterns) #"\s+"))
                       (str/split line #"[|]"))) (str/split-lines s)))

(def input (parse-input (slurp (io/resource "2021/08.txt"))))
(def input-sample (parse-input (slurp (io/resource "2021/08.sample.txt"))))

(defn part-one [rows]
  (reduce (fn [acc row]
            (+ acc (count (filter (fn [s] (contains? #{2 3 4 7} (count s))) (second row)))))
          0 rows))

(def digit-code
  "Each segment can be uniquely identified by the sum of the frequencies of the segments in
  the signal patterns"
  {42 0
   17 1
   34 2
   39 3
   30 4
   37 5
   41 6
   25 7
   49 8
   45 9})

(defn decode-signal [signal-patterns]
  (let [segment-freq (frequencies (str/join signal-patterns))]
    (into {}
          (map (fn [pattern]
                 [(set pattern)
                  (get digit-code (reduce + (map (fn [segment] (get segment-freq segment)) pattern)))])
               signal-patterns))))

(defn decode-patterns [patterns]
  (let [[signal-patterns output-patterns] patterns
        decoder (decode-signal signal-patterns)]
    (Long/parseLong (str/join (map (comp decoder set) output-patterns)))))

(defn part-two [rows] (reduce + (map read-patterns rows)))

(comment
  (part-one input-sample)
  (part-one input)
  (part-two input-sample)
  (part-two input)
  )
