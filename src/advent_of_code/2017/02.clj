(ns advent-of-code.2017.02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn read-input [resource]
  (map (fn [line] (map (fn [s] (Long/parseLong s)) (str/split line #"\s+")))
       (str/split-lines (slurp resource))))


(defn checksum [row-fn]
  (fn  [spreadsheet]
    (reduce
     (fn [acc row] (+ acc (row-fn row)))
     0
     spreadsheet)))


;; Part One

(def checksum-min-max
  (checksum (fn [row] (- (apply max row) (apply min row)))))

(defn test-checksum-min-max []
  (let [f checksum-min-max]
    (assert (= (f [[5 1 9 5]
                   [7 5 3]
                   [2 4 6 8]])
               18))))

(def input-part-one (read-input (io/resource "2017/02/input.txt")))


;; Part Two

(defn evenly-divides? [a b]
  (if (>= a b) (zero? (mod a b)) (zero? (mod b a))))

(defn dividing-pair [[a & ys]]
  (when-let [b (first (drop-while (fn [x] (not (evenly-divides? x a))) ys))]
    (if (> a b) [a b] [b a])))

(defn find-evenly-dividing-pair [xs]
  (->> (repeat (count xs) xs)
       (map-indexed #(drop %1 %2))
       (map dividing-pair)
       (drop-while nil?)
       (first)))

(def checksum-evenly-dividing
  (checksum (fn [row]
              (if-let [pair (find-evenly-dividing-pair row)]
                (apply / pair)
                0))))

(defn test-checksum-evenly-dividing []
  (let [f checksum-evenly-dividing]
    (assert (= (f [[5 9 2 8]
                   [9 4 7 3]
                   [3 8 6 5]])
               9))))

(def input-part-two
  (read-input (io/resource "2017/02/input-2.txt")))
