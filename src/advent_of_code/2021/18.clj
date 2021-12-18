(ns advent-of-code.2021.18
  "Day 18: Snailfish"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.zip :as z]))

(def input (mapv read-string (str/split-lines (slurp (io/resource "2021/18.txt")))))
(def input-sample (mapv read-string (str/split-lines (slurp (io/resource "2021/18.sample.txt")))))

(defn magnitude [p]
  (let [[a b] p]
    (+ (* 3 (if (coll? a) (magnitude a) a))
       (* 2 (if (coll? b) (magnitude b) b)))))

(defn root-loc [loc]
  (if-let [p (z/up loc)]
    (recur p)
    loc))

(defn node-to-explode [z]
  (first
   (filter (fn [z] (and (>= (count (z/path z)) 4)
                       (vector? (z/node z))
                       (number? (first (z/node z)))
                       (number? (second (z/node z)))))
           (take-while (fn [n] (not (z/end? n)))
                       (iterate z/next z)))))

(defn prev-num [z]
  (loop [z z]
    (when-let [z (z/prev z)]
      (if (number? (z/node z))
        z
        (recur z)))))

(defn next-num [z]
  (loop [z z]
    (when-let [z (z/next z)]
      (cond
        (z/end? z) nil
        (number? (z/node z)) z
        :else (recur z)))))

(defn explode [p]
  (when-let [to-explode (node-to-explode p)]
    (let [[l r] (z/node to-explode)
          z (z/replace to-explode 0)
          z (if-let [loc-left (prev-num z)]
              (next-num (z/edit loc-left + l))
              z)
          z (if-let [loc-right (next-num z)]
              (z/edit loc-right + r)
              z)]
      (root-loc z))))

(defn node-to-split [z]
  (first
   (filter (fn [z] (and (number? (z/node z))
                       (>= (z/node z) 10)))
           (take-while (fn [n] (not (z/end? n)))
                       (iterate z/next z)))))

(defn split-number [n]
  [(int (Math/floor (/ n 2))) (int (Math/ceil (/ n 2)))])

(defn split [z]
  (when-let [to-split (node-to-split z)]
    (let [v (z/node to-split)
          z (z/edit to-split split-number)]
      (root-loc z))))

(defn reduce-snailfish [z]
  (if-let [z (explode z)]
    (recur z)
    (if-let [z (split z)]
      (recur z)
      z)))

(defn add-snailfish [n1 n2]
  (z/root (reduce-snailfish (z/vector-zip [n1 n2]))))

(comment
  ;; Part One
  (magnitude (reduce add-snailfish input))
  ;; Part Two
  (apply max (for [a input b input :when (not= a b)] (magnitude (add-snailfish a b))))
  )
