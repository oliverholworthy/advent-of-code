(ns advent-of-code.2018.21
  "Day 21: Chronal Conversion"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [advent-of-code.2018.19 :refer [parse-program step]]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/21/input.txt"))))

(defn find-reg0 [r5 r2]
  (let [r4 (bit-and r2 0xFF)
        r5+ (bit-and 0xFFFFFF
                     (* 65899
                        (bit-and 0xFFFFFF
                                 (+ r4 r5))))]
    (if (< r2 0x100)
      r5+
      (find-reg0 r5+ (quot r2 0x100)))))

(defn part-one [input]
  (find-reg0 input 0x10000))

(defn part-two [input]
  (loop [seen {}
         r5 0]
    (let [r2 (bit-or r5 0x10000)
          r5+ (find-reg0 input r2)]
      (if (get seen r5+)
        r5
        (recur (assoc seen r5+ true) r5+)))))

(comment
  (part-one 10362650) ;; => 6778585
  (part-two 10362650) ;; => 6534225
  )

