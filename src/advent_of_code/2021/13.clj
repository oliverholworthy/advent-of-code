(ns advent-of-code.2021.13
  "Day 13: Transparent Origami"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [s]
  (let [[dot-lines _ fold-lines] (partition-by empty? (str/split-lines s))]
    {:dots (mapv (fn [line] (mapv (fn [c] (Long/parseLong c)) (str/split line #","))) dot-lines)
     :folds (mapv (fn [line] (let [[_ axis v] (re-find #"fold along ([xy])=(\d+)" line)]
                             {:axis (keyword axis)
                              :value (Long/parseLong v)})) fold-lines)}))

(def input (parse-input (slurp (io/resource "2021/13.txt"))))
(def input-sample (parse-input (slurp (io/resource "2021/13.sample.txt"))))

(defn reflect [v d] (if (<= v d) v (- v (* 2 (- v d)))))

(defn fold-dot [axis d [x y]]
  [(if (= axis :x) (reflect x d) x)
   (if (= axis :y) (reflect y d) y)])

(defn fold-dots [dots axis d]
  (vec (distinct (map (partial fold-dot axis d) dots))))

(defn part-one [{:keys [dots folds]}]
  (let [{:keys [axis value]} (first folds)]
    (fold-dots dots axis value)))

(defn print-dots [dots]
  (let [max-x (reduce max (map first dots))
        max-y (reduce max (map second dots))
        dots-set (set dots)]
    (println
     (str/join
      "\n"
      (for [y (range (inc max-y))]
        (str/join (map (fn [x] (if (contains? dots-set [x y]) "#" "."))
                       (range (inc max-x)))))))))

(defn part-two [{:keys [dots folds]}]
  (reduce (fn [dots {:keys [axis value]}] (fold-dots dots axis value)) dots folds))

(comment
  (part-one input)
  (print-dots (part-two input))
  )
