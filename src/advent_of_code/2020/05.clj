(ns advent-of-code.2020.05
  "Day 5: Binary Boarding"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (str/split-lines
   (slurp (io/resource "2020/05/input.txt"))))

(defn space-partition [code n l-char]
  "binary space partition of size n with left branch character l-char"
  (first (reduce
          (fn [[l u] c]
            (let [next-row (+ (/ (- u l) 2) l)]
              (if (= c l-char) [l next-row] [next-row u])))
          [0 n]
          code)))

(defn parse-seat [seat]
  "parse seat code"
  (let [row (space-partition (subs seat 0 7) 128 \F)
        col (space-partition (subs seat 7) 8 \L)]
    {:row row
     :col col
     :id (+ (* row 8) col)}))

(defn first-missing [xs]
  "First gap of 2 in a list of integers"
  (reduce (fn [prev-x x]
            (if (and prev-x x (= (- x prev-x) 2))
              (reduced (inc prev-x))
              x))
          (sort xs)))

(comment
  ;; Part One
  (apply max (map :id (map parse-seat input)))
  ;; Part Two
  (first-missing (map :id (map parse-seat input)))
  )
