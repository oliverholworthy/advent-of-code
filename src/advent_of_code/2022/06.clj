(ns advent-of-code.2022.06
  "Day 6: Tuning Trouble"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (first (str/split-lines (slurp (io/resource "2022/06.txt")))))

(defn packet-marker-index [buffer n-unique]
  "Number of characters need to be processed before the first packet marker is detected. the
   packet marker is determined based on a number of unique characters occuring in order."
  (->> buffer
       (partition n-unique 1 buffer)
       (map-indexed (fn [i v] [i (count (set v))]))
       (drop-while (fn [[i v]] (not= n-unique v)))
       (ffirst)
       (+ n-unique)))

(comment
  (packet-marker-index input 4)
  (packet-marker-index input 14)
  )
