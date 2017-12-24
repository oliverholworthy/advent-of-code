(ns advent-of-code.2017.23
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input [resource]
  (->> (slurp resource)
       (str/trim)
       (str/split-lines)
       (map (fn [line]
              (let [[f a b] (str/split line #"\s+")]
                (case f
                  "set" [:set {:register a
                               :value b}]
                  "sub" [:sub {:register a
                               :value b}]
                  "mul" [:mul {:register a
                               :value b}]
                  "jnz" [:jnz {:register a :value b}]))))))

(defn get-val [registers v]
  (if (re-matches #"\-?\d+" v)
    (Long/parseLong v)
    (get registers v 0)))

(defn apply-instructions [operations]
  (let [n (count operations)]
    (loop [invoke-count 0
           op-count {}
           i 0
           registers {}]
      (if (or (>= i n) (< i 0) (> invoke-count 1e5))
        {:i i
         :invoke-count invoke-count
         :op-count op-count
         :registers registers}
        (let [[operation {:keys [register value]}] (nth operations i)
              x (get-val registers register)
              y (get-val registers value)]
          (recur
           (inc invoke-count)
           (update op-count operation (fn [v] (inc (or v 0))))
           (if (and (= operation :jnz) (not (zero? x)))
             (+ i y)
             (inc i))
           (case operation
             :set (assoc registers register y)
             :sub (assoc registers register (- x y))
             :mul (assoc registers register (* x y))
             registers)))))))

(defn is-prime [x]
  (or (<= 2 x 3)
      (and (pos? x)
           (not (zero? (mod x 2)))
           (not (zero? (mod x 3)))
           (every? false?
                   (for [i (range 5 (Math/sqrt x) 6)]
                     (or (zero? (mod x i))
                         (zero? (mod x (+ i 2)))))))))

;; -----------------------------------------------------------------------------

(comment

  ;; Part One

  (def instructions (read-input (io/resource "2017/23/input.txt")))

  (get-in (apply-instructions instructions)
          [:op-count :mul])
  ;; => 6241


  ;; Part Two

  (let [b 81
        b (+ 100000 (* 100 b))
        c (+ b (* 17 1000))]
    (count (filter (complement is-prime)
                   (range b
                          (inc c)
                          17))))
  ;; => 909
  )
