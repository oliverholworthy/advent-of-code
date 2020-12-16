(ns advent-of-code.2020.16
  "Day 16: Ticket Translation"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (str/split-lines
   (slurp (io/resource "2020/16/input.txt"))))

(defn from-str-range [str-range]
  (let [[_ l u] (re-matches #"(\d+)-(\d+)" str-range)]
    (range (Long/parseLong l) (inc (Long/parseLong u)))))

(defn parse-input [input]
  (reduce (fn [acc line]
            (cond
              (empty? line) acc
              (= "your ticket:" line) (assoc acc :parse :your-ticket)
              (= "nearby tickets:" line) (assoc acc :parse :nearby-tickets)
              (= (:parse acc) :rules)
              (let [[_ k a b] (re-matches #"(.*): (.*) or (.*)" line)]
                (assoc-in acc [:rules k] (set (concat (from-str-range a)
                                                      (from-str-range b)))))
              (= (:parse acc) :your-ticket)
              (assoc acc :your-ticket (mapv #(Long/parseLong %) (str/split line #",")))
              (= (:parse acc) :nearby-tickets)
              (update acc :nearby-tickets
                      (fn [n] (conj (or n [])
                                   (mapv #(Long/parseLong %) (str/split line #",")))))
              :else acc))
          {:parse :rules}
          input))

(defn part-one [input]
  "ticket scanning error rate. sum of all invalid values"
  (let [state (parse-input input)
        all-valid-values (apply set/union  (vals (:rules state)))]
    (reduce +
            (mapcat (fn [ticket] (remove (fn [val] (contains? all-valid-values val)) ticket))
                    (:nearby-tickets state)))))

(defn reduce-ticket-options [ticket-options]
  "find intersection of ticket options across a list of tickets"
  (reduce
   (fn [acc t]
     (if acc
       (map (fn [a b] (set/intersection a b)) acc t)
       t))
   nil
   ticket-options))

(defn find-mapping [field-options]
  "Find the unique mapping of indices to values provided a list of sets of values."
  (loop [options-by-index (into {} (map-indexed vector field-options))
         field-mapping {}]
    (let [[k-single v-single] (first (filter (fn [[k v]] (= 1 (count v))) options-by-index))]
      (if (nil? k-single)
        field-mapping
        (recur (into {} (map (fn [[k v]] [k (disj v (first v-single))]) options-by-index))
               (assoc field-mapping k-single (first v-single)))))))

(defn possible-values [rules ticket]
  "Get set of possible rules for each element in ticket"
  (map (fn [ticket-value]
         (set (map first
                   (filter (fn [[rule-name rule-values]]
                             (contains? rule-values ticket-value))
                           rules))))
       ticket))

(defn part-two [input]
  "Product of departure fields on ticket"
  (let [{:keys [rules nearby-tickets your-ticket]} (parse-input input)
        all-valid-values (apply set/union  (vals rules))
        valid-nearby-tickets
        (remove (fn [ticket] (seq (remove (fn [val] (contains? all-valid-values val)) ticket)))
                nearby-tickets)
        field-mapping
        (->> valid-nearby-tickets
             (map (partial possible-values rules))
             (reduce-ticket-options)
             (find-mapping))]
    (->> field-mapping
         (filter (fn [[i field-name]] (str/starts-with? field-name "departure")))
         (map (fn [[i _]] (get your-ticket i)))
         (apply *))))

(comment
  (part-one input)
  (part-two input)
  )
