(ns advent-of-code.2022.05
  "Day 5: Supply Stacks"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/split-lines (slurp (io/resource "2022/05.txt"))))
(def input-sample (str/split-lines (slurp (io/resource "2022/05.sample.txt"))))

;; -----------------------------------------------------------------------------
;; Parse input data

(defn parse-stack [stack-lines]
  (let [n (int (/ (count (first stack-lines)) 4))]
    (->> stack-lines
         (reduce (fn [stack line]
                   (reduce (fn [stack i]
                             (update stack (inc i)
                                     (fn [xs] (let [x (nth line (inc (* i 4)) nil)]
                                               (if (Character/isLetter x)
                                                 (cons x (or xs '()))
                                                 xs)))))
                           stack
                           (range (inc n))))
                 {})
         (map (fn [[k v]] [k (reverse v)]))
         (into {}))))

(defn parse-moves [move-lines]
  (map (fn [line]
         (let [[_ quantity from to] (re-matches #"move (\d+) from (\d+) to (\d+)" line)]
           {:quantity (Long/parseLong quantity)
            :from (Long/parseLong from)
            :to (Long/parseLong to)}))
       move-lines))

(defn parse-input [input-lines]
  (let [[stack-lines _ move-lines] (partition-by empty? input-lines)]
    {:moves (parse-moves move-lines)
     :stack (parse-stack stack-lines)}))

;; -----------------------------------------------------------------------------
;; Crate Moving

(defn apply-move [one-by-one? stack move]
  (let [{:keys [quantity from to]} move
        moved-crates (take quantity (get stack from))
        moved-crates (if one-by-one? (reverse moved-crates) moved-crates)]
    (-> stack
        (update from (partial drop quantity))
        (update to (partial concat moved-crates)))))

(defn move-crates [one-by-one?]
  (let [{:keys [stack moves]} (parse-input input)]
    (reduce (partial apply-move one-by-one?) stack moves)))

(defn top-of-stack [stack]
  (str/join (map (comp first #(get stack %)) (sort (keys stack)))))

(defn part-one [input]
  "Returns top of each stack after moving crates one by one."
  (top-of-stack (move-crates true)))

(defn part-two [input]
  "Returns top of each stack after moving crates together."
  (top-of-stack (move-crates false)))

(comment
  (part-one input)
  (part-two input)
  )
