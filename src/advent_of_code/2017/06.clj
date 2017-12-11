(ns advent-of-code.2017.06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn max-loc [banks]
  (reduce
   (fn [[acc-i acc-bank] [i bank]]
     (if (> bank acc-bank)
       [i bank]
       [acc-i acc-bank]))
   [0 0]
   (map-indexed vector banks)))

(defn redistribute [banks]
  (let [[loc-i loc-bank] (max-loc banks)
        n (count banks)
        bank-quot (quot loc-bank n)
        b (if (zero? bank-quot) loc-bank (rem loc-bank (* n bank-quot)))]
    (vec (map-indexed (fn [i bank]
                        (+ (if (= i loc-i) 0 bank)
                           bank-quot
                           (let [i2 (mod (+ i (- n loc-i)) n)]
                             (if (and (<= i2 b) (> i2 0))
                               1 0))))
                      banks))))

(defn memory-allocation
  [banks]
  (loop [configurations (hash-set banks)
         banks banks
         steps 0]
    (let [new-banks (redistribute banks)]
      (if (contains? configurations new-banks)
        [(inc steps) banks]
        (recur (conj configurations new-banks)
               new-banks
               (inc steps))))))

(def input
  (->> (slurp (io/resource "2017/06/input.txt"))
       (str/trim )
       (#(str/split % #"\s+"))
       (mapv #(Long/parseLong %))))

;; Part One

(defn cycles [banks]
  (first (memory-allocation banks)))

;; Part Two

(defn loop-size
  [banks]
  (first (memory-allocation (second (memory-allocation banks)))))
