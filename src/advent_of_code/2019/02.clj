(ns advent-of-code.2019.02
  (:require [clojure.string :as str]))

(def input "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,1,23,13,27,1,6,27,31,1,9,31,35,2,10,35,39,1,39,6,43,1,6,43,47,2,13,47,51,1,51,6,55,2,6,55,59,2,59,6,63,2,63,13,67,1,5,67,71,2,9,71,75,1,5,75,79,1,5,79,83,1,83,6,87,1,87,6,91,1,91,5,95,2,10,95,99,1,5,99,103,1,10,103,107,1,107,9,111,2,111,10,115,1,115,9,119,1,13,119,123,1,123,9,127,1,5,127,131,2,13,131,135,1,9,135,139,1,2,139,143,1,13,143,0,99,2,0,14,0")

(defn parse-input [s] (mapv #(Long/parseLong %) (str/split s #",")))

(def program (parse-input input))

(defn step [state position]
  (let [opcode (get state position)
        new-state (fn [op] (let [a (get state (inc position)) b (get state (+ position 2))  i (get state (+ position 3))]
                            (assoc state i (op (get state a) (get state b)))))]
    (case opcode
      1 [(new-state +) (+ 4 position)]
      2 [(new-state *) (+ 4 position)]
      99 [state position])))

(defn run [program]
  (loop [program program
         position 0
         steps 0]
    (let [[new-state new-pos] (step program position)]
      (if (= new-pos position)
        [program steps]
        (recur new-state new-pos (inc steps))))))

(defn run-init [memory noun verb]
  (ffirst (run (assoc memory 1 noun 2 verb))))

(comment
  ;; Part One
  (run-init program 12 2) ;; => 7210630

  ;; Part Two
  (->> (for [noun (range 100)
             verb (range 100)]
         [noun verb])
       (filter (fn [[noun verb]] (= 19690720 (run-init program noun verb))))) ;; => ([38 92])
  )
