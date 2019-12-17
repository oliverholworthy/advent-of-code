(ns advent-of-code.2019.14
  "Day 14: Space Stoichiometry"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (slurp (io/resource "2019/14/input.txt")))

(defn parse-amount [s]
  (let [[_ v c] (re-matches #"(\d+) ([A-Z]+)" s)]
    [c (Long/parseLong v)]))

(defn parse-line [s] (let [[from to] (str/split s #"=>")]
                       {:to (parse-amount (str/trim to))
                        :from (into {} (map (comp parse-amount str/trim) (str/split from #",")))}))


(defn parse-input [input]
  (reduce (fn [acc {:keys [to from]}]
            (assoc acc (first to)
                   {:produces (second to)
                    :from from}))
          {}
          (map parse-line (str/split-lines input))))

(defn get-ore [reactions element fuel]
  (if (= element "FUEL")
    fuel
    (reduce + (map (fn [[from-element {:keys [from produces]}]]
                     (* (get from element)
                        (Math/ceil (/ (get-ore reactions from-element fuel)
                                      produces))))
                   (filter (fn [[_ {:keys [from produces]}]]
                             (contains? from element))
                           reactions)))))

(defn part-two [reactions fuel-min fuel-max ore-max]
  (loop [fuel-min fuel-min
         fuel-max fuel-max]
    (if (zero? (dec (- fuel-max fuel-min)))
      fuel-min
      (let [avg (quot (+ fuel-max fuel-min) 2)
            qty (get-ore reactions "ORE" avg)]
        (recur (if (<= qty ore-max) avg fuel-min)
               (if (<= qty ore-max) fuel-max avg))))))

(comment
  (let [reactions (parse-input input)]
    (get-ore reactions "ORE" 1)) ;; => 579797.0

  (let [reactions (parse-input input)]
    (part-two reactions 1 100e6 1e12)) ;; => 2521844.0
  )
