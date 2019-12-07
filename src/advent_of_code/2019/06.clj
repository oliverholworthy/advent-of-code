(ns advent-of-code.2019.06
  "Day 6: Universal Orbit Map"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input
  (map str/trim
       (str/split-lines
        (slurp (io/resource "2019/06/input.txt")))))

(defn parse-line [line] (str/split line #"\)"))

(defn parse-input [input]
  (reduce (fn [acc [a b]]
            (update acc a (fn [v] (conj (or v []) b))))
          {}
          (map parse-line input)))

(defn count-orbits* [k {:keys [path orbits]}]
  (if-let [v (get orbits k)]
    (reduce + (map (fn [k] (+ 1 (count path) (count-orbits* k {:path (conj path k) :orbits orbits}))) v))
    0))

(defn total-orbit-count
  "count all orbits indirect + direct"
  [orbits]
  (count-orbits* "COM" {:orbits orbits :path []}))

(defn paths* [k {:keys [path orbits]}]
  (if-let [v (get orbits k)]
    (if (> (count v) 1)
      (mapv (fn [k] (path-to* k {:path (conj path k) :orbits orbits})) v)
      (path-to* (first v) {:path (conj path (first v)) :orbits orbits}))
    path))

(defn paths
  "return a list of paths to all leaf planets (without any orbiting planets)"
  [orbits]
  (filter
   (fn [x] (and (sequential? x) (string? (first x))))
   (tree-seq
    sequential?
    seq
    (paths* "COM" {:orbits orbits :path ["COM"]}))))

(defn you-to-santa [orbits]
  ""
  (let [all-paths (paths orbits)
        path-to-santa (first (filter (fn [p] (= (last p) "SAN")) all-paths))
        path-to-you (first (filter (fn [p] (= (last p) "YOU")) all-paths))
        shared-path-count (count (take-while (fn [[p1 p2]] (= p1 p2)) (map vector path-to-santa path-to-you)))]
    (+ (- (count path-to-santa) shared-path-count)
       (- (count path-to-you) shared-path-count)
       -2)))

(comment
  (total-orbit-count (parse-input input)) ;; => 333679
  (you-to-santa (parse-input input)) ;; => 370
  )
