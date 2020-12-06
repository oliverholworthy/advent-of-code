(ns advent-of-code.2020.06
  "Day 6: Custom Customs"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (str/split-lines
   (slurp (io/resource "2020/06/input.txt"))))

(defn count-customs [lines count-fn]
  (reduce + (map count-fn (partition-by empty? lines))))

(defn part-one [lines]
  (count-customs lines (fn [group] (count (set (str/join group))))))

(defn part-two [lines]
  (count-customs lines (fn [group] (count (apply set/intersection (map set group))))))
