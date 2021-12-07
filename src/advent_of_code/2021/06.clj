(ns advent-of-code.2021.06
  "Day 6: Lanternfish"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (mapv #(Long/parseLong %) (str/split (str/trim (slurp (io/resource "2021/06.txt"))) #",")))

(defn run [h]
  (assoc (reduce (fn [acc i] (assoc acc i (get acc (inc i) 0)))
                 (-> h
                     (update 7 + (get h 0))
                     (update 9 + (get h 0)))
                 (range 9))
         9 0))

(defn count-generation [start g]
  (let [days-left-freq (frequencies start)
        hist (mapv (fn [i] (get days-left-freq i 0)) (range 10))]
    (reduce + (nth (iterate run hist) g))))

(comment
  (count-generation input 80)
  (count-generation input 256)
 )
