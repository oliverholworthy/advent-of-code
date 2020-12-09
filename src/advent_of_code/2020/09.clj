(ns advent-of-code.2020.09
  "Day 9: Encoding Error"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (map #(Long/parseLong %)
       (str/split-lines
        (slurp (io/resource "2020/09/input.txt")))))

(defn contains-sum? [xs n]
  "Does a list of numbers XS contain two numbers that sum to N"
  (let [set-xs (into #{} xs)]
    (not (every? (fn [x] (not (contains? (disj set-xs x) (- n x)))) xs))))

(defn find-invalid-number [xs step]
  "Finds the first number in the list XS which is NOT the sum of two
  of the STEP numbers before it."
  (reduce (fn [acc ys]
            (let [[n & prev] (reverse ys)]
              (when-not (contains-sum? prev n)
                (reduced n))))
   nil
   (partition-all (inc step) 1 xs)))

(defn find-contiguous-range-sum [xs target]
  "Find a contiguous range of at least two numbers in list XS which
  sum to the TARGET"
  (let [[start {:keys [sum end]}]
        (first
         (reduce
          (fn [acc [i x]]
            (cond->
                (reduce (fn [new-acc [k {:keys [sum end] :as v}]]
                          (cond-> new-acc
                            (<= (+ sum x) target)
                            (assoc k {:sum (+ sum x) :end i})
                            (= sum target)
                            (assoc k v)))
                        {} acc)
                (<= x target)
                (assoc i {:sum x :end i})))
          {}
          (map-indexed vector xs)))]
    (when (= sum target)
      (->> xs (drop start) (take (inc ( - end start)))))))

(defn find-weakness [xs step]
  "Find encryption weakness. Sum of the smallest and largest numbers
  in a contiguous range. That sums to the invalid number."
  (let [invalid-number (find-invalid-number xs step)
        weakness-range (find-contiguous-range-sum xs invalid-number)]
    (when (seq weakness-range)
      (+ (apply min weakness-range)
         (apply max weakness-range)))))

(comment
  ;; Part One
  (find-invalid-number input 25)

  ;; Part Two
  (find-weakness input 25)
  )
