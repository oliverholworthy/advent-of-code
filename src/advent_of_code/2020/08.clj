(ns advent-of-code.2020.08
  "Day 8: Handheld Halting"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (str/split-lines
   (slurp (io/resource "2020/08/input.txt"))))

(def input-sample
  (str/split-lines
   (slurp (io/resource "2020/08/input-sample.txt"))))

(defn parse-input [input]
  (vec
   (map-indexed
    (fn [i line]
      (let [[_ instr argument] (re-matches #"(.*) ([+-]\d+)" line)]
        {:line i :instruction (keyword instr) :argument (Long/parseLong argument)}))
    input)))

(defn run [program]
  (loop [visited #{}
         accumulator 0
         line-number 0]
    (let [{:keys [instruction argument]} (get program line-number)]
      (cond (contains? visited line-number)
            {:exit :cycle :accumulator accumulator}
            (not (and instruction argument))
            {:exit :success :accumulator accumulator}
            :else
            (recur (conj visited line-number)
                   (if (= instruction :acc) (+ accumulator argument) accumulator)
                   (if (= instruction :jmp) (+ line-number argument) (inc line-number)))))))

(defn switch-line [program line]
  (assoc program (:line line)
         (cond-> line
           (= :jmp (:instruction line))
           (assoc :instruction :nop)
           (= :nop (:instruction line))
           (assoc :instruction :jmp))))

(defn find-program-bug [input]
  "try switching jmp and nop instructions to find "
  (let [program (parse-input input)
        lines-to-try (filter (comp #{:jmp :nop} :instruction) program)]
    (filter (fn [[{:keys [exit]}]] (= exit :success))
            (map (juxt (comp run (partial switch-line program)) identity)
                 lines-to-try))))

(comment
  ;; Part One
  (run (parse-input input-sample))
  (run (parse-input input))

  ;; Part Two
  (find-program-bug input-sample)
  (find-program-bug input)
  )

