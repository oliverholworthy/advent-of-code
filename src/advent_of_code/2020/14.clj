(ns advent-of-code.2020.14
  "Day 14: Docking Data"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (str/split-lines
   (slurp (io/resource "2020/14/input.txt"))))

(defn parse-line [line]
  (case (subs line 0 3)
    "mem"
    (let [[_ address value] (re-matches #"mem\[(\d+)\] = (\d+)" line)]
      {:instruction :mem :address (Long/parseLong address) :value (Long/parseLong value)})
    "mas"
    (let [[_ mask] (re-matches #"mask = ([01X]+)" line)]
      {:instruction :mask :mask mask})))

(defn left-pad-with-zeros [s n]
  (str/join [(str/join (repeat (- n (count s)) "0")) s]))

(defn to-binary [x]
  "convert number to binary string and pad to 36 chars with leading zeros"
  (left-pad-with-zeros (Long/toBinaryString x) 36))

(defn combine-mask [bit-mask x]
  "mask number with bit-mask. ignoring places with X"
  (str/join (map (fn [m v] (if (= m \X) v m)) bit-mask (to-binary x))))

(defn run [input]
  (reduce (fn [acc instr]
            (case (:instruction instr)
              :mask (assoc acc :mask (:mask instr))
              :mem (assoc-in acc [:memory (:address instr)]
                             (Long/parseLong (combine-mask (:mask acc) (:value instr)) 2))
              acc))
          {}
          (map parse-line input)))

(defn part-one [input]
  (reduce + (vals (:memory (run input)))))

(defn fill-floating [bit-str]
  "Expand floating values X in a bit string returning all combinations"
  (let [x-pos (map first (filter #(= (second %) \X) (map-indexed (fn [i v] [i v]) bit-str)))
        n (count x-pos)
        x-var (map #(left-pad-with-zeros (Long/toBinaryString %) n) (range (Math/pow 2 n)))]
    (map (fn [xs]
           (str/join (reduce (fn [s [x-i s-i]] (assoc s s-i (nth xs x-i)))
                             (vec bit-str)
                             (map-indexed vector x-pos))))
         x-var)))

(defn combine-mask-2 [bit-mask x]
  "mask number with bit-mask. ignoring places with 0 and floating value for X"
  (fill-floating
   (str/join (map (fn [m v] (if (= m \0) v m)) bit-mask (to-binary x)))))

(defn run-2 [input]
  (reduce (fn [acc instr]
            (case (:instruction instr)
              :mask (assoc acc :mask (:mask instr))
              :mem (reduce
                    (fn [acc address] (assoc-in acc [:memory address] (:value instr)))
                    acc
                    (map #(Long/parseLong % 2)
                         (combine-mask-2 (:mask acc) (:address instr)))) 
              acc))
          {}
          (map parse-line input)))

(defn part-two [input]
  (reduce + (vals (:memory (run-2 input)))))

(comment
  (part-one input)
  (part-two input)
)
