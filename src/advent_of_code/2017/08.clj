(ns advent-of-code.2017.08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def operators
  {">=" >=
   "<" <
   "!=" not=
   "<=" <=
   "==" ==
   ">" >})

(def modifiers
  {"inc" +
   "dec" -})

(defn parse-line [line]
  (let [[_ register inc-dec change-val
         condition-register condition-operator condition-val]
        (re-find #"([a-z]+) (.*) (-?\d+) if ([a-z]+) (.*) (-?\d+)" line)]
    {:register register
     :modify (fn [x] ((get modifiers inc-dec) x (Long/parseLong change-val)))
     :val (Long/parseLong change-val)
     :condition {:register condition-register
                 :operator (get operators condition-operator)
                 :val (Long/parseLong condition-val)}}))

(defn parse-input [resource]
  (->> (slurp resource)
       (str/trim)
       (str/split-lines)
       (map parse-line)))

(defn instruction-registers [instructions]
  (into {} (map (fn [r] [r 0]) (distinct (map :register instructions)))))

(defn apply-instruction [registers instruction]
  (let [{:keys [condition register val modify]} instruction]
    (if ((:operator condition)
         (get registers (:register condition))
         (:val condition))
      (update registers register modify)
      registers)))

(defn highest-register-val [registers]
  (second (first (sort-by second > registers))))

(defn apply-instructions [instructions]
  (loop [registers (instruction-registers instructions)
         instructions instructions
         highest 0]
    (if (empty? instructions)
      {:registers registers
       :highest-during highest
       :highest (highest-register-val registers)}
      (let [new-registers
            (apply-instruction registers (first instructions))
            new-highest (highest-register-val new-registers)]
        (recur new-registers
               (rest instructions)
               (if (> new-highest highest)
                 new-highest
                 highest))))))

(def input (parse-input (io/resource "2017/08/input.txt")))
(def input-sample (parse-input (io/resource "2017/08/input-sample.txt")))
