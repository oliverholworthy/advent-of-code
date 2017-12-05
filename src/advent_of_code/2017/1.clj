(ns advent-of-code.2017.1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/trim (slurp (io/resource "2017/1/input.txt"))))

(defn captcha [numbers]
  (let [xs (map #(Long/parseLong (str %)) (str numbers))]
    (:res (reduce
           (fn [{:keys [res prev]} x]
             {:res (if (and prev (= prev x))
                     (+ res x)
                     res)
              :prev x})
           {:res 0
            :prev nil}
           (cons (last xs) xs)))))

(defn test-captcha []
  (assert (= (captcha 1122) 3))
  (assert (= (captcha 1111) 4))
  (assert (= (captcha 1234) 0))
  (assert (= (captcha 91212129) 9)))
