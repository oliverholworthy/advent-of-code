(ns advent-of-code.2017.4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn valid? [passphrase]
  (let [words (str/split passphrase #" ")]
    (= (count (set words)) (count words))))

(def input-part-one (str/split-lines (str/trim (slurp (io/resource "2017/4/input-1.txt")))))

;; (count (filter valid? input-part-one))
;; => 451
