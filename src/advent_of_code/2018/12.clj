(ns advent-of-code.2018.12
  "Day 12: Subterranean Sustainability"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/12/input.txt"))))

(def input-lines-sample
  (str/split-lines
   (slurp (io/resource "2018/12/input-sample.txt"))))

(defn parse-initial-state [s]
  (map-indexed
   (fn [i c]
     {:number i
      :plant (= (str c) "#")
      :repr (str c)})
   (second (re-find #"initial state: (.*)" s))))

(defn parse-rule [s]
  (let [[_ ptn dst]
        (re-find #"^(.*) => (.*)$" s)]
    [ptn dst]))

(defn parse-input [lines]
  (let [initial-state (parse-initial-state (first lines))
        rules (into {}
                    (map parse-rule
                         (remove empty? (rest lines))))]
    {:state initial-state
     :rules rules}))

(def init-data-sample (parse-input input-lines-sample))
(def init-data (parse-input input-lines))

(defn extend-state [state]
  (let [n 5
        l-count
        (- n
           (count (take-while (complement :plant) state)))
        r-count
        (- n (count (take-while (complement :plant)
                                (reverse state))))
        l-num (:number (first state))
        r-num (:number (last state))
        l (mapv (fn [n] {:number n :plant false :repr "."})
                (range (- l-num l-count) l-num))
        r (mapv (fn [n] {:number n :plant false :repr "."})
                (range (inc r-num) (+ (inc r-num) r-count)))]
    (vec (concat l state r))))

(defn tick [{:keys [rules state] :as acc}]
  (assoc acc
         :state
         (map (fn [xs]
                (let [new-plant
                      (get rules (str/join (map :repr xs)))]
                  (if (= new-plant "#")
                    (assoc (nth xs 2) :repr "#" :plant true)
                    (assoc (nth xs 2) :repr "." :plant false))))
              (partition 5 1 (extend-state state)))))

(defn part-one [input gen]
  (->> input
       (iterate tick)
       (drop gen)
       (first)
       (:state)
       (filter :plant)
       (map :number)
       (reduce +)))

(defn plant-pot-sum [{:keys [state]}]
  (->> state
       (filter :plant)
       (map :number)
       (reduce +)))

(defn part-two [input search-gen target-gen]
  (loop [i 0
         d input
         c (plant-pot-sum input)
         diffs []]
    (if (< i search-gen)
      (let [d2 (tick d)
            c2 (plant-pot-sum d2)]
        (recur (inc i)
               d2
               c2
               (conj diffs (- c2 c))))
      (let [repeated-incr (ffirst (sort-by second > (frequencies diffs)))]
        (+ (* repeated-incr (- target-gen i))
           c)))))

(comment
  (def input (parse-input input-lines))
  (part-one input 20) ;; => 3798
  (part-two input 2e3 50000000000) ;; => 3900000002212
  )


