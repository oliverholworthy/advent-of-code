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
  (let [op-str (format "%05d" op-code)]
    {:opcode (Long/parseLong (subs op-str (- (count op-str) 2)))
     :parameter-modes (mapv (fn [c] (case c \1 :immediate \0 :position :position)) (reverse (subs op-str 0 (- (count op-str) 2))))}))

(defn step [state position]
  (let [{:keys [opcode parameter-modes]} (read-op-code (get state position))
        get-value (fn [mode v] (case mode
                                :immediate v
                                :position (get state v)))
        get-params (fn [] [(get-value (get parameter-modes 0) (get state (inc position)))
                          (get-value (get parameter-modes 1) (get state (+ position 2)))
                          (get state (+ position 3))])]
    (case opcode
      1 [(let [[a b i] (get-params)]
           (assoc state i (+ a b)))
         (+ 4 position)]
      2 [(let [[a b i] (get-params)]
           (assoc state i (* a b)))
         (+ 4 position)]
      3 (let [v (Long/parseLong (read-line))
              i (get state (inc position))]
          [(assoc state i v) (+ 2 position)])
      4 (let [[v] (get-params)]
          [(do (println v) state) (+ 2 position)])
      5 (let [[a b] (get-params)]
          (if (not (zero? a))
            [state b]
            [state (+ position 3)]))
      6 (let [[a b] (get-params)]
          (if (zero? a)
            [state b]
            [state (+ position 3)]))
      7 [(let [[a b i] (get-params)]
           (assoc state i (if (< a b) 1 0)))
         (+ position 4)]
      8 [(let [[a b i] (get-params)]
           (assoc state i (if (= a b) 1 0)))
         (+ position 4)]
      99 [state position])))

(defn run [program]
  (loop [program program
         position 0
         steps 0]
    (let [[new-state new-pos] (step program position)]
      (if (and (= new-state program) (= new-pos position))
        [program steps]
        (recur new-state new-pos (inc steps))))))

(comment
  (run program)
  )
