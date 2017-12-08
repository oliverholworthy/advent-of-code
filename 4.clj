(ns advent-of-code.2017.4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Part One

(def input-passphrases (str/split-lines (str/trim (slurp (io/resource "2017/4/passphrases.txt")))))

(defn valid-words? [passphrase]
  (let [words (str/split passphrase #" ")]
    (= (count (set words)) (count words))))

;; (count (filter valid-words? input-passphrases))
;; => 451


;; Part Two

(defn valid-anagrams? [passphrase]
  (let [words (str/split passphrase #" ")]
    (= (count (set (map set words))) (count (map set words)))))

;; (count (filter valid-anagrams? input-passphrases))
;; => 223
