(ns advent-of-code.2021.12
  "Day 12: Passage Pathing"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse-input [s] (mapv (fn [line] (map keyword (str/split line #"-"))) (str/split-lines s)))
(def input (parse-input (slurp (io/resource "2021/12.txt"))))
(def input-sample (parse-input (slurp (io/resource "2021/12.sample.txt"))))

(defn to-graph [edges]
  (dissoc (reduce (fn [acc [x y]]
                    (-> acc
                        (update x (fn [xs] (conj (or xs #{}) y)))
                        (update y (fn [ys] (conj (or ys #{}) x)))))
                  {}
                  edges)
          :end))

(defn paths [G small-cave-max-visit]
  (let [small-caves (into #{} (filter (fn [n] (= (name n) (str/lower-case (name n)))) (keys G)))]
    ((fn explore [frontier]
       (lazy-seq
        (when-let [[path total-cost] (peek frontier)]
          (let [v (last path)
                neighbours (get G v)]
            (cons [v total-cost path]
                  (explore
                   (merge-with
                    (fn [val-in-result val-in-latter]
                      (first (sort-by second [val-in-result val-in-latter])))
                    (pop frontier)
                    (into {}
                          (for [n (remove
                                   (fn [node]
                                     (or (= node :start)
                                         (and (contains? small-caves node)
                                              (let [node-freq (frequencies path)
                                                    max-small-cave-freq (apply max (map #(get node-freq % 0) small-caves))]
                                                (>= (get node-freq node 0)
                                                    (if (<= max-small-cave-freq 1) small-cave-max-visit 1))))))
                                   neighbours)]
                            {(conj path n) (inc total-cost)})))))))))
     (priority-map [:start] 0))))

(defn part-one [edges] (count (filter #(= :end (first %)) (paths (to-graph edges) 1))))

(defn part-two [edges] (count (filter #(= :end (first %)) (paths (to-graph edges) 2))))

(comment
  (part-one input)
  (part-two input)
  )
