(ns advent-of-code.2018.01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-part-one
  (str/split-lines
   (slurp (io/resource "2018/01/input-1.txt"))))

(defn parse-long [x] (Long/parseLong x))

(defn part-one [xs]
  (reduce + (map parse-long xs)))

(part-one input-part-one)

;; -------------------------------------------------------------------------

(defn part-two [xs]
  (reduce (fn [{:keys [freq freqs iter]} x]
            (let [new-freq (+ freq x)
                  new-acc
                  {:freq new-freq
                   :iter (inc iter)
                   :freqs (conj freqs new-freq)}]
              (if (contains? freqs new-freq)
                (reduced new-freq)
                new-acc)))
          {:freq 0
           :iter 0
           :freqs #{0}}
          (cycle (map parse-long xs))))

(part-two input-part-one)
(part-two (str/split "+7, +7, -2, -7, -4" #", "))
(part-two (str/split "-6, +3, +8, +5, -6" #", "))
(part-two (str/split "+3, +3, +4, -2, -4" #", "))
(part-two (str/split "+1, -1" #", "))
(part-two (str/split "+1, -2, +3, +1" #", "))
