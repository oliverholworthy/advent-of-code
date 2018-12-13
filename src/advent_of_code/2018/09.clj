(ns advent-of-code.2018.09
  "Day 9: Marble Mania"
  (:require [clojure.string :as str])
  (:import [java.util LinkedList]))

;; [-] (0)
;; [1]  0 (1)
;; [2]  0 (2) 1
;; [3]  0  2  1 (3)
;; [4]  0 (4) 2  1  3
;; [5]  0  4  2 (5) 1  3
;; [6]  0  4  2  5  1 (6) 3
;; [7]  0  4  2  5  1  6  3 (7)

(defn rotate-right [lst n]
  (dotimes [n n] (.add lst 0 (.removeLast lst))))

(defn rotate-left [lst n]
  (dotimes [n n] (.add lst (.removeFirst lst))))

(defn place-marbles [num-players marble-count]
  (let [circle (LinkedList. [0])]
    (loop [marble 1
           scores {}]
      (cond
        (> marble marble-count) scores

        (zero? (mod marble 23))
        (do (rotate-right circle 7)
            (let [removed-marble (.removeLast circle)
                  scores (update scores (mod marble num-players) (fn [s] (+ (or s 0) marble removed-marble)))]
              (rotate-left circle 1)
              (recur (inc marble) scores)))

        :else
        (do (rotate-left circle 1)
            (.add circle marble)
            (recur (inc marble) scores))))))

(defn max-score [scores]
  (apply max (vals scores)))

(comment
  (max-score (place-marbles 10 1618))
  (max-score (place-marbles 21 6111))
  (time (def res-one (max-score (place-marbles 459 72103))))
  (time (def res-two (max-score (place-marbles 459 (* 100 72103)))))
  )
