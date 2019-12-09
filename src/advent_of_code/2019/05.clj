(ns advent-of-code.2019.05
  "Day 5: Sunny with a Chance of Asteroids"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input
  (str/split-lines
   (slurp (io/resource "2019/05/input.txt"))))

(defn parse-input [s] (mapv #(Long/parseLong %) (str/split s #",")))

(def program (parse-input (first input)))

(defn read-op-code [op-code]
  (let [op-str (format "%05d" (int op-code))]
    {:opcode (Long/parseLong (subs op-str (- (count op-str) 2)))
     :parameter-modes (mapv (fn [c] (case c \2 :relative \1 :immediate \0 :position :position))
                            (reverse (subs op-str 0 (- (count op-str) 2))))}))

(read-op-code 203)

(defn step [{:keys [mem position inputs outputs relative-base] :as state}]
  (let [{:keys [opcode parameter-modes]} (read-op-code (get mem position))
        get-value (fn [mode pos]
                    (case mode
                      :position (get mem (get mem pos))
                      :immediate (get mem pos)
                      :relative (get mem (+ (get mem pos) relative-base))))
        get-position (fn [mode pos]
                       (case mode
                         :relative (+ (get mem pos) relative-base)
                         :position (get mem pos)))
        params [(get-value (get parameter-modes 0) (+ position 1))
                (get-value (get parameter-modes 1) (+ position 2))
                (get-position (get parameter-modes 2) (+ position 3))]]
    (case opcode
      1 (assoc state
               :mem
               (let [[a b i] params]
                 (assoc mem i (+ a b)))
               :position
               (+ 4 position))
      2 (assoc state
               :mem
               (let [[a b i] params]
                 (assoc mem i (* a b)))
               :position
               (+ 4 position))
      3 (if-let [v (first inputs)]
          (let [i (get-position (get parameter-modes 0) (inc position))]
            (assoc state :inputs (rest inputs) :mem (assoc mem i v) :position (+ 2 position)))
          (assoc state :awaiting-input true))
      4 (assoc state
               :position
               (+ 2 position)
               :outputs
               (let [[v] params]
                 (conj outputs v)))
      5 (assoc state
               :position
               (let [[a b] params]
                 (if (not (zero? a))
                   b
                   (+ position 3))))
      6 (assoc state
               :position
               (let [[a b] params]
                 (if (zero? a)
                   b
                   (+ position 3))))
      7 (assoc state
               :mem
               (let [[a b i] params]
                 (assoc mem i (if (< a b) 1N 0N)))
               :position
               (+ position 4))
      8 (assoc state
               :mem
               (let [[a b i] params]
                 (assoc mem i (if (= a b) 1N 0N)))
               :position
               (+ position 4))
      9 (let [[a] params] (assoc state :relative-base (+ relative-base a) :position (+ position 2)))
      99 (assoc state :halted true))))

(defn run [state]
  (loop [state state
         steps 0]
    (let [new-state (step state)]
      (if (or (:halted new-state) (:awaiting-input new-state))
        new-state
        (recur new-state (inc steps))))))

(defn init-state [program inputs]
  {:mem (mapv bigint (concat program (repeat 1e6 0))) :position 0 :inputs inputs :outputs [] :relative-base 0N})

(comment
  (:outputs (run (init-state program [1])))
  (:outputs (run (init-state program [5])))
  )
