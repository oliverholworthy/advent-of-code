(ns advent-of-code.2019.01 "Day 1: The Tyranny of the Rocket Equation"
    (:require [clojure.java.io :as io]
              [clojure.string :as str]))

(def input
  (str/split-lines
   (slurp (io/resource "2019/01/input.txt"))))

(defn fuel-requirement [mass]
  (max 0 (- (int (/ mass 3)) 2)))

(defn fuel-requirement-incl-fuel [mass]
  (if (zero? mass)
    mass
    (let [fuel-mass (fuel-requirement mass)]
      (+ fuel-mass (fuel-requirement-incl-fuel fuel-mass)))))

(defn total-fuel-requirement [module-masses fuel-fn]
  (->> module-masses
       (map #(Long/parseLong %))
       (map fuel-fn)
       (reduce +)))

(comment
  (total-fuel-requirement input fuel-requirement) ;; => 3232358
  (total-fuel-requirement input fuel-requirement-incl-fuel) ;; => 4845669
  )
