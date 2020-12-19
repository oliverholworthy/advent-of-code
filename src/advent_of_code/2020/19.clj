(ns advent-of-code.2020.19
  "Day 19: Monster Messages"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.combinatorics :refer [cartesian-product]]))

(def input
  (str/split-lines
   (slurp (io/resource "2020/19/input.txt"))))

(defn parse-rule [rule]
  (let [[_ v] (re-matches #"\"([a-z]+)\"" rule)]
    (if v v (let [sub-rules (str/split rule #"\s[|]\s")]
              (mapv (fn [sub-rules] (str/split sub-rules #"\s+")) sub-rules)))))

(defn parse-input [input]
  (reduce (fn [acc line]
            (cond (re-matches #"(\d+): (.*)" line)
                  (let [[_ rule-id rule] (re-matches #"(\d+): (.*)" line)]
                    (assoc-in acc [:rules rule-id] (parse-rule rule)))
                  (re-matches #"[a-z]+" line)
                  (update acc :messages (fn [xs] (conj (or xs []) line)))
                  :else acc))
          {}
          input))

(defn expand-rules [rules rule-id]
  "expands rules to get a list of all valid messages"
  (let [rule (get rules rule-id)]
    (if (string? rule)
      (list rule)
      (mapcat (fn [sub-rule]
                (map str/join
                     (apply cartesian-product
                            (map (partial expand-rules rules) sub-rule))))
              rule))))

(defn regex-match [rules n rule-id]
  "Expand rules to regex pattern"
  (let [rule (get rules rule-id)]
    (cond
      (> n 16) "" ;; max loop depth
      (string? rule) rule
      :else
      (str "("
           (str/join "|"
                     (map (fn [sub-rule]
                            (str/join (map (partial regex-match rules (inc n)) sub-rule)))
                          rule))
           ")"))))

(comment
  ;; Part One
  (let [{:keys [rules messages]} (parse-input input)
        ptn (re-pattern (regex-match rules 0 "0"))]
    (count (filter (fn [msg] (re-matches ptn msg)) messages)))

  ;; Part Two
  (let [{:keys [rules messages]} (parse-input input)
        rules (-> rules
                  (assoc "8" [["42"] ["42" "8"]])
                  (assoc "11" [["42" "31"] ["42" "11" "31"]]))
        ptn (re-pattern (regex-match rules 0 "0"))]
    (count (filter (fn [msg] (re-matches ptn msg)) messages)))
  )
