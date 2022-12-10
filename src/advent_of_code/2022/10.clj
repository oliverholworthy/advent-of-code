(ns advent-of-code.2022.10
  "Day 10: Cathode-Ray Tube"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/split-lines (slurp (io/resource "2022/10.txt"))))
(def input-sample (str/split-lines (slurp (io/resource "2022/10.sample.txt"))))

(defn run-instructions [instructions]
  (:cycles
   (reduce (fn [{:keys [cycles register]} instruction]
             (let [add-instruction? (str/starts-with? instruction "addx")
                   add-val (when add-instruction?
                             (Long/parseLong (second (str/split instruction #"\s+"))))]
               {:cycles (cond-> (conj cycles register)
                          add-instruction?
                          (conj register))
                :register (cond-> register
                             add-instruction?
                             (+ add-val))}))
           {:cycles []
            :register 1}
           instructions)))

(defn signal-strengths [cycles]
  (let [n (count cycles)]
    (map (fn [i] (* i (get cycles (dec i)))) (range 20 n 40))))

(defn part-one
  "Sum of signal strengths after running instructions"
  [instructions]
  (reduce + (signal-strengths (run-instructions instructions))))

(defn part-two
  "Print out image on CRT from input instructions"
  [instructions]
  (println)
  (->> (run-instructions instructions)
       (map-indexed
        (fn [crt-pixel sprite-position]
          (let [diff (- (mod crt-pixel 40) (mod sprite-position 40))]
            (if (and (<= diff 1)
                     (>= diff -1))
              \#
              \.))))
       (partition 40)
       (map str/join)
       (str/join "\n")
       (println)))

(comment
  (part-one input)
  (part-two input)
  )
