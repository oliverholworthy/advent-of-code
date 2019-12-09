(ns advent-of-code.2019.09
  "Day 9: Sensor Boost"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [advent-of-code.2019.05 :refer [run init-state]]))

(def input (concat (str/split (str/trim (slurp (io/resource "2019/09/input.txt"))) #",")
                   (repeat 1000 "0")))

(comment
  (:outputs (run (init-state (concat (str/split "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" #",") (repeat 1000 "0")) [])))
  (:outputs (run (init-state (str/split "1102,34915192,34915192,7,4,7,99,0" #",") [])))
  (:outputs (run (init-state (str/split "104,1125899906842624,99" #",") [])))
  (:outputs (run (init-state input  [1]))) ;; => [3638931938N]
  (:outputs (run (init-state input [2]))) ;; => [86025N]
  )
