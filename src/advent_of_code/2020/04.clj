(ns advent-of-code.2020.04
  "Day 4: Passport Processing"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (str/split-lines
   (slurp (io/resource "2020/04/input.txt"))))

(defn parse-line [line]
  (into {} (map #(let [[k v] (str/split % #":")]
                   [(keyword k) v])
                (remove empty? (str/split line #"\s+")))))

(defn parse-input [lines]
  (let [res (reduce
             (fn [{:keys [passports cur-passport]} line]
               (let [passport-details (parse-line line)]
                 (if (empty? passport-details)
                   {:passports (conj passports cur-passport)
                    :cur-passport {}}
                   {:passports passports
                    :cur-passport (merge cur-passport passport-details)})))
             {:passports [] :cur-passport {}}
             lines)]
    (cond-> (:passports res)
      (not (empty? (:cur-passport res)))
      (conj (:cur-passport res)))))

(defn is-between? [number-str lower upper]
  (and number-str
       (<= (Long/parseLong number-str) upper)
       (>= (Long/parseLong number-str) lower)))

(defn is-valid-year? [year lower upper]
  (let [[_ year-match] (re-matches #"^(\d\d\d\d)$" year)]
    (is-between? year-match lower upper)))

(defn is-valid-height? [height]
  (let [[_ height-val height-schema]
        (re-matches #"^(\d+)(in|cm)$" height)]
    (case height-schema
      "in" (is-between? height-val 59 76)
      "cm" (is-between? height-val 150 193)
      false)))

(defn is-valid-eye-color? [eye-color]
  (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
             eye-color))

(defn is-valid-hair-color? [hair-color]
  (not (nil? (re-matches #"#([0-9a-f]{6})" hair-color))))

(defn is-valid-passport-id? [passport-id]
  (not (nil? (re-matches #"^([0-9]{9})$" passport-id))))

(def validation-fns
  {:byr #(is-valid-year? % 1920 2002)
   :iyr #(is-valid-year? % 2010 2020)
   :eyr #(is-valid-year? % 2020 2030)
   :hgt is-valid-height?
   :hcl is-valid-hair-color?
   :ecl is-valid-eye-color?
   :pid is-valid-passport-id?})

(defn passport-is-valid? [passport]
  "part one. validation of keys only"
  (= (disj (set (keys passport)) :cid)
     #{:byr :iyr :eyr :hgt :hcl :ecl :pid}))

(defn passport-is-valid-2? [passport]
  "part two. validation of keys and values"
  (let [validation-keys #{:byr :iyr :eyr :hgt :hcl :ecl :pid}]
    (and (= (disj (set (keys passport)) :cid)
            validation-keys)
         (every? (fn [k] ((k validation-fns) (k passport))) validation-keys))))

(comment
  (frequencies (map passport-is-valid? (parse-input input)))
  (frequencies (map passport-is-valid-2? (parse-input input)))
  )
