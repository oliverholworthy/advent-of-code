(ns advent-of-code.2021.02
  "Day 2: Dive!"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (mapv (fn [line] (let [[_ dir x] (re-matches #"([a-z]+) (\d+)" line)]
                             {:dir (keyword dir) :x (Long/parseLong x)}))
                (str/split-lines (slurp (io/resource "2021/02.txt")))))

(defn part-one [xs]
  (let [[final-pos final-depth]
        (reduce (fn [[pos depth] {:keys [dir x]}]
                  (case dir
                    :forward [(+ pos x) depth]
                    :down [pos (+ depth x)]
                    :up [pos (- depth x)]
                    [pos depth]))
                [0 0] xs)]
    (* final-pos final-depth)))

(defn part-two [xs]
  (let [[final-pos final-depth final-aim]
        (reduce (fn [[pos depth aim] {:keys [dir x]}]
                  (case dir
                    :forward [(+ pos x) (+ depth (* aim x)) aim]
                    :down [pos depth (+ aim x)]
                    :up [pos depth (- aim x)]
                    [pos depth aim]))
                [0 0 0] xs)]
    (* final-pos final-depth)))

(comment
  (part-one input)
  (part-two input)
  )
