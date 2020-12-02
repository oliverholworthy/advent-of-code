(ns advent-of-code.2020.02
  "Day 2: Password Philosophy"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (str/split-lines
   (slurp (io/resource "2020/02/input.txt"))))

(defn parse-line [line]
  (let [[_ lower upper char password]
        (re-matches #"(\d+)-(\d+) ([a-z]): ([a-z]+)" line)]
    {:policy {:lower (Long/parseLong lower)
              :upper (Long/parseLong upper)
              :char (first char)}
     :password password}))

(defn is-valid-a? [{:keys [policy password]}]
  "Policy based on interpreting upper and lower as bounds for char frequencies"
  (let [{:keys [lower upper char]} policy
        char-freqs (frequencies password)
        policy-char-count (get char-freqs char 0)]
    (and (<= policy-char-count upper)
         (>= policy-char-count lower))))

(defn bool-to-int [b] (if b 1 0))

(defn is-valid-b? [{:keys [policy password]}]
  "Policy based on interpreting upper and lower as positions in
  the password that must contain the character in
  Exactly one of the positions not both"
  (let [{:keys [lower upper char]} policy
        char-matches-lower (bool-to-int (= char (get password (dec lower))))
        char-matches-upper (bool-to-int (= char (get password (dec upper))))]
    (= 1 (bit-xor char-matches-lower char-matches-upper))))

(comment
  (is-valid-a? (parse-line "1-3 a: abcde"))
  (is-valid-a? (parse-line "1-3 b: cdefg"))
  (is-valid-a? (parse-line "2-9 c: ccccccccc"))
  ;; Part One
  (count (filter identity (map (comp is-valid-a? parse-line) input)))

  (is-valid-b? (parse-line "1-3 a: abcde"))
  (is-valid-b? (parse-line "1-3 b: cdefg"))
  (is-valid-b? (parse-line "2-9 c: ccccccccc"))

  ;; Part Two
  (count (filter identity (map (comp is-valid-b? parse-line) input)))
  )
