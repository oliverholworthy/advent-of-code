(ns advent-of-code.2019.16
  "Day 16: Flawed Frequency Transmission"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (mapv #(Long/parseLong (str %)) (str/trim (slurp (io/resource "2019/16/input.txt")))))

(def pattern
  (memoize
   (fn [n]
     (cycle (reduce (fn [acc x] (concat (repeat n x) acc)) '() [-1 0 1 0])))))

(defn get-value [lst i]
  (let [ptn (rest (pattern (inc i)))]
    (Math/abs (rem (reduce + (map (fn [x1 x2] (* x1 x2)) lst ptn)) 10))))

(defn phase [lst]
  (reduce (fn [acc i]
            (conj acc (get-value lst i)))
          []
          (range (count lst))))

(defn phase2 [lst]
  (reductions (fn [a b] (mod (+ a b) 10)) lst))

(comment
  ;; Part One
  (nth (iterate phase [1 2 3 4 5 6 7 8]) 100)
  (str/join (take 8 (nth (iterate phase input) 100))) ;; => "96136976"

  ;; Part Two
  (let [offset (Long/parseLong (str/join (take 7 input)))
        input2 (reverse (drop offset (flatten (repeat 10000 input))))]
    (str/join (take 8 (reverse (nth (iterate phase2 input2) 100)))))
  )
