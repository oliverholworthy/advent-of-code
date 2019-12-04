(ns advent-of-code.2019.04
  "Day 4: Secure Container"
  (:require [clojure.set :as set]))

(defn valid? [password]
  (let [digits (map (fn [c] (Integer/parseInt (str c))) password)]
    (and (= 6 (count digits))
         (every? (fn [[x1 x2]] (>= x2 x1)) (partition 2 1 digits))
         (boolean (seq (filter (fn [[x1 x2]] (= x1 x2)) (partition 2 1 digits)))))))

(defn valid-2? [password]
  (let [digits (map (fn [c] (Integer/parseInt (str c))) password)
        pairs (set (map first (filter (fn [[x1 x2]] (= x1 x2)) (partition 2 1 digits))))
        triples (set (map first (filter (fn [[x1 x2 x3]] (= x1 x2 x3)) (partition 3 1 digits))))]
    (and (= 6 (count digits))
         (every? (fn [[x1 x2]] (>= x2 x1)) (partition 2 1 digits))
         (boolean (seq (set/difference pairs triples))))))

(comment
  ;; Part One
  (count (filter (comp valid? str) (range 206938 679128))) ;; => 1653

  ;; Part Two
  (count (filter (comp valid-2? str) (range 206938 679128))) ;; => 1133
  )
