(ns advent-of-code.2018.23
  "Day 23: Experimental Emergency Teleportation"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/23/input.txt"))))

(def input-lines-sample
  (str/split-lines
   (slurp (io/resource "2018/23/input-sample.txt"))))

(defn parse-line [line]
  (let [[_ & xs]
        (re-find #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)"
                 line)
        [px py pz r] (mapv #(Long/parseLong %) xs)]
    {:p [px py pz]
     :r r}))

(defn manhattan-distance
  [[^long x1 ^long y1 ^long z1]
   [^long x2 ^long y2 ^long z2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))
     (Math/abs (- z2 z1))))

(defn part-one [lines]
  (let [points (mapv parse-line lines)
        strongest (first (sort-by :r > points))]
    (count
     (filter (fn [p]
               (<= (manhattan-distance (:p p) (:p strongest))
                   (:r strongest)))
             points))))

(comment
  (part-one input-lines)
  )
