(ns advent-of-code.2020.07
  "Day 7: Handy Haversacks"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (str/split-lines
   (slurp (io/resource "2020/07/input.txt"))))

(def input-sample
  (str/split-lines
   (slurp (io/resource "2020/07/sample.txt"))))

(defn parse-inner-count [inner-count]
  (let [[_ bag-count bag-type] (re-matches #"(\d+) (.*) bag.*" (str/trim inner-count))]
    {:count (Long/parseLong bag-count) :type bag-type}))

(defn parse-inner [inner]
  (if (= inner "no other bags.") []
      (map parse-inner-count
           (str/split inner #","))))

(defn parse-line [line]
  (let [[_ outer inner] (re-matches #"(.*) bags contain (.*)" line)]
    [outer (parse-inner inner)]))

(defn contains-map [input reverse]
  (reduce (fn [acc [outer-type inner]]
            (reduce (fn [a i]
                      (assoc-in a (if reverse [(:type i) outer-type] [outer-type (:type i)])
                                (:count i)))
                    acc inner))
          {}
          (map parse-line input)))

(defn bag-contains-all [contains bag]
  "bags containing at least one bag"
  (let [xs (keys (get contains bag {}))]
    (if (seq xs)
      (concat xs (mapcat (partial bag-contains-all contains) xs))
      (list))))

(defn bag-contains-count [contains bag]
  "number of bags containing bag"
  (let [xs (keys (get contains bag {}))
        vs (vals (get contains bag {}))]
    (if-not (empty? xs)
      (concat vs (map (fn [x v] (reduce + (map #(* v %) (bag-contains-count contains x)))) xs vs))
      (list))))

(comment
  ;; Part One
  (count (set (bag-contains-all (contains-map input true) "shiny gold")))
  (count (set (bag-contains-all (contains-map input-sample true) "shiny gold")))

  ;; Part Two
  (apply + (bag-contains-count (contains-map input false) "shiny gold"))
  (apply + (bag-contains-count (contains-map input-sample false) "shiny gold"))
  )
