(ns advent-of-code.2019.07
  "Day 7: Amplification Circuit"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :refer [permutations]]
            [advent-of-code.2019.05 :refer [run init-state]]))

(def program (mapv #(Long/parseLong %) (str/split (str/trim (slurp (io/resource "2019/07/input.txt"))) #",")))

(defn run-phase-settings [program phase-settings]
  (loop [inputs [0]
         amps (mapv (fn [phase-setting] (init-state program [phase-setting])) phase-settings)]
    (if (empty? amps)
      (last inputs)
      (let [amp (first amps)
            new-amp (run (-> amp (dissoc :awaiting-input) (update :inputs concat inputs)))
            new-amps (cond-> (vec (rest amps)) (not (:halted new-amp)) (conj new-amp))]
        (recur [(last (:outputs new-amp))]
               new-amps)))))

(defn highest-signal [program lb ub]
  (first (sort-by second > (map (juxt identity (partial run-with-feedback program)) (permutations (range lb ub))))))

(comment
  (highest-signal program 0 5) ;; => [[4 2 1 0 3] 34686]
  (highest-signal program 5 10) ;; => [[7 6 5 8 9] 36384144]
  )
