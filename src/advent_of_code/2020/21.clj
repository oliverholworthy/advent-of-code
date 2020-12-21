(ns advent-of-code.2020.21
  "Day 21: Allergen Assessment"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (str/split-lines (slurp (io/resource "2020/21/input.txt"))))

(def input-sample
  (str/split-lines (slurp (io/resource "2020/21/sample.txt"))))

(defn parse-line [line]
  (let [[_ ingredients possible-allergens]
        (re-matches #"([a-z ]+) \(contains ([a-z, ]+)\)" line)]
    {:ingredients (str/split ingredients #"\s")
     :allergens (str/split possible-allergens #", ")}))

(defn find-allergens [xs]
  "find allergen"
  (reduce (fn [acc {:keys [allergens ingredients]}]
            (reduce (fn [allergen-mapping allergen]
                      (update allergen-mapping allergen
                              (fn [ingrs]
                                (set/intersection
                                 (or ingrs (set ingredients))
                                 (set ingredients)))))
                    acc allergens))
          {}
          xs))

(defn find-unique-mapping
  "Find the unique mapping of keys to values provided a mapping to sets of values."
  [options]
  (loop [options options
         field-mapping {}]
    (let [[k-single v-single]
          (first (filter (fn [[k v]] (= 1 (count v)))
                         options))]
      (if (nil? k-single)
        field-mapping
        (recur (into {} (map (fn [[k v]] [k (disj v (first v-single))])
                             options))
               (assoc field-mapping k-single (first v-single)))))))

(defn part-one
  "How many times do ingredients without allergens appear"
  [input]
  (let [foods (map parse-line input)
        allergen-mapping
        (find-unique-mapping (find-allergens foods))
        ingredient-freq (frequencies (mapcat :ingredients foods))]
    (reduce +
            (vals (reduce (fn [acc allergen] (dissoc acc allergen))
                          ingredient-freq
                          (vals allergen-mapping))))))

(defn part-two
  "list of dangerous ingredients (contains allergen)
  in alphabetical order"
  [input]
  (let [foods (map parse-line input)
        allergen-mapping
        (find-unique-mapping (find-allergens foods))]
    (str/join "," (map second (sort-by first allergen-mapping)))))

(comment
  (part-one input-sample)
  (part-one input)
  (part-two input-sample)
  (part-two input)
  )
