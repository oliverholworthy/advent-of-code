(ns advent-of-code.2021.14
  "Day 14: Extended Polymerization"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [s]
  (let [[polymer-template _ pair-insertion-lines] (partition-by empty? (str/split-lines s))]
    {:polymer-template (first polymer-template)
     :pair-insertion-rules (into {} (map (fn [line] (let [[pair target] (str/split line #" -> ")]
                                                     [(into [] pair) (first target)]))
                                         pair-insertion-lines))}))

(def input (parse-input (slurp (io/resource "2021/14.txt"))))
(def input-sample (parse-input (slurp (io/resource "2021/14.sample.txt"))))

(defn add-v [v x] (+ x (or v 0)))

(defn insertion [rules template]
  (reduce (fn [acc [[c1 c2] n]]
            (if-let [c (get rules [c1 c2])]
              (-> acc
                  (update [c1 c] add-v n)
                  (update [c1 c2] (fn [v] (- v n)))
                  (update [c c2] add-v n))
              acc))
          template
          template))

(defn polymer-freqs-n [polymer n]
  (let [{:keys [polymer-template pair-insertion-rules]} polymer
        template (frequencies (map vec (partition 2 1 (cons "_" polymer-template))))
        pair-freqs (nth (iterate (partial insertion pair-insertion-rules) template) n)]
    (reduce (fn [acc [[c1 c2] c]] (update acc c2 add-v c)) {} pair-freqs)))

(defn freq-diff [polymer n]
  (let [element-freq (polymer-freqs-n polymer n)
        sorted-freq (sort-by second element-freq)
        [[_ most-common-freq] [_ least-common-freq]]
        [(last sorted-freq) (first sorted-freq)]]
    (- most-common-freq least-common-freq)))

(comment
  (freq-diff input 10)
  (freq-diff input 40)
  )
