(ns advent-of-code.2018.19
  "Day 19: Go With The Flow"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code.2018.16 :refer [opcodes]]))

(def input-lines-sample
  (str/split-lines
   (slurp (io/resource "2018/19/input-sample.txt"))))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/19/input.txt"))))

(def instruction-by-name (into {} (map (juxt :name :fn) opcodes)))

(defn parse-line [line]
  (let [[fn-name a b c] (str/split line #" ")]
    {:fn-name fn-name
     :fn (get instruction-by-name fn-name)
     :args (mapv #(bigint (Long/parseLong %)) [a b c])}))

(defn parse-program [lines]
  {:instruction-pointer
   (bigint (Long/parseLong (second (re-find #"ip (\d)" (first lines)))))
   :instructions
   (mapv parse-line (rest lines))})

(defn step [{:keys [instruction-pointer
                    instructions
                    registers]
             :as acc}]
  (if-let [instruction (get instructions (get registers instruction-pointer))]
    (let [new-registers (assoc registers 0 instruction-pointer)
          new-registers ((:fn instruction) registers (:args instruction))]
      (-> acc
          (assoc :instruction [(:fn-name instruction) (:args instruction)])
          (assoc :registers new-registers)
          (update-in [:registers instruction-pointer] inc)))
    (assoc acc :halt true)))

(defn part-one [lines]
  (->> (-> (parse-program lines)
           (assoc :registers [0 0 0 0 0 0]))
       (iterate step)
       (drop-while (complement :halt))
       (first)
       (:registers)
       (first)))

(defn part-two [lines]
  (->> (-> (parse-program lines)
           (assoc :registers [1 0 0 0 0 0]))
       (iterate step)
       (drop 2000)
       (first)
       (:registers)
       (sort >)
       (first)
       (sum-of-factors)))

(defn sum-of-factors [x]
  (reduce + (filter (fn [v] (zero? (mod x v)))
                    (range 1 (inc x)))))

(comment
  (part-one input-lines-sample)
  (part-one input-lines)
  (part-two input-lines)
  )
