(ns advent-of-code.2018.05
  "Day 5: Alchemical Reduction"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/05/input.txt"))))

(defn react? [c1 c2]
  (and (not= c1 c2)
       (= (str/lower-case c1) (str/lower-case c2))))

(defn reaction [s]
  (str/join (reduce
             (fn [acc c]
               (let [prev-c (peek acc)]
                 (if (and prev-c (react? c prev-c))
                   (pop acc)
                   (conj acc c))))
             []
             s)))

(defn fully-reacted [[s1 s2]] (= s1 s2))

(def polymer (first input-lines))

(defn fully-react [polymer]
  (->> polymer
       (iterate reaction)
       (partition 2 1)
       (take-while (complement fully-reacted))
       (last)
       (last)))

(defn part-one [polymer]
  (count (fully-react polymer)))

;; -------------------------------------------------------------------------

(defn remove-unit [unit s]
  (str/join (remove (fn [c] (or (= (str c) (str unit))
                               (= (str c) (str/upper-case unit))))
                    s)))

(defn part-two [polymer]
  (let [units (distinct (map str/lower-case polymer))]
    (->> units
         (map (fn [unit]
                [unit (count (fully-react (remove-unit unit polymer)))]))
         (sort-by second <)
         (first))))
