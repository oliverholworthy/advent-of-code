(ns advent-of-code.2017.4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Part One

(def input-passphrases
  (str/split-lines (str/trim (slurp (io/resource "2017/4/passphrases.txt")))))

(defn to-words [passphrase] (str/split passphrase #" "))

(defn get-valid-fn [word-fn]
  (fn [passphrase]
    (let [words (to-words passphrase)]
      (= (count (word-fn words))
         (count words)))))

(def valid-words? (get-valid-fn set))

;; (count (filter valid-words? input-passphrases))
;; => 451


;; Part Two

(def valid-anagrams? (get-valid-fn (comp set (partial map set))))

;; (count (filter valid-anagrams? input-passphrases))
;; => 223
