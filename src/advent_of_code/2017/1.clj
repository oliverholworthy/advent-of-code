(ns advent-of-code.2017.1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn captcha-circular [steps-fn]
  (fn [numbers]
    (let [xs (map #(Long/parseLong (str %)) (str numbers))
          steps (steps-fn xs)]
      (:res (reduce
             (fn [{:keys [res next]} x]
               {:res (if (= (first next) x)
                       (+ res x)
                       res)
                :next (rest next)})
             {:res 0
              :next (drop steps (cycle xs))}
             xs)))))


;; Part One

(def captcha-next (captcha-circular (fn [xs] (inc (count xs)))))

(defn test-captcha-next []
  (let [f captcha-next]
    (assert (= (f 1122) 3))
    (assert (= (f 1111) 4))
    (assert (= (f 1234) 0))
    (assert (= (f 91212129) 9))))

(def input-part-one (str/trim (slurp (io/resource "2017/1/input-1.txt"))))


;; Part Two

(def captcha-halfway (captcha-circular (fn [xs] (/ (count xs) 2))))

(defn test-captcha-halfway []
  (let [f captcha-halfway]
    (assert (= (f 1212) 6))
    (assert (= (f 1221) 0))
    (assert (= (f 123425) 4))
    (assert (= (f 123123) 12))
    (assert (= (f 12131415) 4))))

(def input-part-two (str/trim (slurp (io/resource "2017/1/input-2.txt"))))
