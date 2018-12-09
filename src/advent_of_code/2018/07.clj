(ns advent-of-code.2018.07
  "Day 7: The Sum of Its Parts"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/07/input.txt"))))

(def input-lines-sample
  (str/split-lines
   (slurp (io/resource "2018/07/input-sample.txt"))))

(defn parse-line [line]
  (let [[_ x y] (re-find #"Step ([A-Z]) must be finished before step ([A-Z]) can begin." line)]
    [x y]))

(defn ordering [edges]
  (reduce
   (fn [acc [x y]]
     (update acc x (fn [s] (assoc (or s {}) y {}))))
   {}
   edges))

(defn in-freq [edges]
  (reduce (fn [acc [x y]]
            (-> acc
                (update y (fn [c] (inc (or c 0))))
                (update x (fn [c] (or c 0)))))
          {}
          edges))

(defn root-nodes [edges]
  (map first (filter (comp zero? second) (sort-by second < (in-freq edges)))))

(defn construct-graph [tree k]
  (reduce (fn [t [node sub-t]]
            (assoc t node (construct-graph tree node)))
          {}
          (get tree k)))

(defn dag [edges]
  (let [t (ordering edges)]
    (reduce
     (fn [acc n]
       (assoc acc n (construct-graph t n)))
     {}
     (root-nodes edges))))

(defn next-node [{:keys [graph]}]
  "Traverse graph and find next node available in alphabetical order"
  (let [k (first (sort (keys graph)))]
    {:graph (merge (dissoc graph k) (get graph k))
     :node k}))

(defn part-one [g]
  (str/join
   (reverse
    (:res
     (reduce
      (fn [{:keys [seen res]} s]
        {:res (if (contains? seen s) res (conj res s))
         :seen (conj seen s)})
      {:seen #{}
       :res []}
      (reverse (map :node
                    (take-while :node
                                (rest (iterate next-node
                                               {:graph g}))))))))))

(comment
  (part-one (dag (map parse-line input-lines-sample)))
  (part-one (dag (map parse-line input-lines)))
  "BETUFNVADWGPLRJOHMXKZQCISY"
  )
