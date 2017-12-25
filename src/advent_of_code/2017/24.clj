(ns advent-of-code.2017.24
  "Electromagnetic Moat"
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn read-input [resource]
  (map (fn [line]
         (let [[x y] (str/split line #"\/")]
           [(Long/parseLong x) (Long/parseLong y)]))
       (str/split-lines (slurp resource))))

(defn bridges [bridge port components]
  (let [next-components (filter (fn [[a b]] (or (= a port) (= b port)))
                                components)]
    (if (empty? next-components)
      [bridge]
      (mapcat (fn [[a b]]
                (let [y (if (= a port) b a)]
                  (bridges
                   (conj bridge [a b])
                   y
                   (remove #(= [a b] %) components))))
              next-components))))

(defn strength [bridge] (reduce + (flatten bridge)))

(defn max-stength [bridges]
  (reduce (fn [max-strength bridge]
            (let [bridge-strength (strength bridge)]
              (if (> bridge-strength max-strength)
                bridge-strength
                max-strength)))
          0
          bridges))

(defn max-strength-longest [bridges]
  (reduce (fn [[max-length max-strength] bridge]
            (let [bridge-length (count bridge)]
              (cond (> bridge-length max-length) [bridge-length (strength bridge)]
                    (= bridge-length max-length)
                    (let [bridge-strength (strength bridge)]
                      (if (> bridge-strength max-strength)
                        [max-length bridge-strength]
                        [max-length max-strength]))
                    :else [max-length max-strength])))
          [0 0]
          bridges))

;; -----------------------------------------------------------------------------

(comment
  (def components (read-input (io/resource "2017/24/input.txt")))
  (def all-bridges (bridges [] 0 components))


  ;; Part One

  (def part-one (max-stength all-bridges))

  ;; Part Two

  (def part-two (second (max-strength-longest all-bridges)))
  ;; => 1471

  )
