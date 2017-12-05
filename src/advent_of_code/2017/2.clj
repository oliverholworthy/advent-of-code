(ns advent-of-code.2017.2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (map (fn [line] (map (fn [s] (Long/parseLong s)) (str/split line #"\s+")))
       (str/split-lines (slurp (io/resource "2017/2/input.txt")))))

(defn checksum [spreadsheet]
  (reduce
   (fn [acc row] (+ acc (- (apply max row) (apply min row))))
   0
   spreadsheet))

(defn test-checksum []
  (assert (= (checksum [[5 1 9 5]
                        [7 5 3]
                        [2 4 6 8]])
             18)))
