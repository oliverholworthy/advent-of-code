(ns advent-of-code.2021.04
  "Day 4: Giant Squid"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (str/split-lines (slurp (io/resource "2021/04.txt"))))
(def input-sample (str/split-lines (slurp (io/resource "2021/04.sample.txt"))))

(defn coerce-long [s] (Long/parseLong s))

(defn parse-input [lines]
  (let [numbers (mapv coerce-long (str/split (first lines) #","))
        boards (map #(mapv (fn [line] (mapv coerce-long (str/split (str/trim line) #"\s+"))) %)
                    (remove #(= 1 (count %)) (partition-by empty? (rest lines))))]
    {:numbers numbers
     :boards boards}))

(defn to-row-cols [board]
  (let [rows board
        cols (apply map vector rows)]
    (concat (map set rows) (map set cols))))

(defn board-nums [row-cols] (set (apply concat row-cols)))

(defn get-winners [{:keys [numbers boards]}]
  (:winners
   (reduce (fn [{:keys [draw winners boards-row-cols] :as acc} x]
             (let [new-draw (conj draw x)
                   nums (set new-draw)
                   new-winners
                   (filter (fn [xs-sets]
                             (first (filter
                                     (fn [xs] (empty? (set/difference xs nums)))
                                     xs-sets)))
                           boards-row-cols)]
               (cond->
                   (assoc acc :draw new-draw)
                   (seq new-winners)
                   (-> (update :winners concat
                               (map (fn [winner] [(conj draw x) winner]) new-winners))
                       (assoc :boards-row-cols
                              (reduce (fn [acc winner]
                                        (remove #(= (board-nums winner) (board-nums %))
                                                acc))
                                      boards-row-cols
                                      new-winners))))))
           {:draw [] :winners [] :boards-row-cols (map to-row-cols boards)}
           numbers)))

(defn part-one [{:keys [numbers boards] :as input}]
  (let [winners (get-winners input)
        [draw winning-board-row-cols] (first winners)
        last-draw (peek draw)
        sum-of-unmarked-nums
        (reduce + (set/difference (board-nums winning-board-row-cols)
                                  (set draw)))]
    (* last-draw sum-of-unmarked-nums)))

(defn part-two [{:keys [numbers boards] :as input}]
  (let [winners (get-winners input)
        [draw winning-board-row-cols] (last winners)
        last-draw (peek draw)
        sum-of-unmarked-nums
        (reduce + (set/difference (board-nums winning-board-row-cols)
                                  (set draw)))]
    (* last-draw sum-of-unmarked-nums)))

(comment
  (part-one (parse-input input-sample))
  (part-one (parse-input input))
  (part-two (parse-input input-sample))
  (part-two (parse-input input))
  )
