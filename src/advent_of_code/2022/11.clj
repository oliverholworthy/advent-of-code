(ns advent-of-code.2022.11
  "Day 11: Monkey in the Middle"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/split-lines (slurp (io/resource "2022/11.txt"))))
(def input-sample (str/split-lines (slurp (io/resource "2022/11.sample.txt"))))

(defn divisible-by? [x divisor] (zero? (mod x divisor)))

(defn parse-monkey [lines]
  (let [s (str/join ", " lines)
        [_ monkey-id]
        (re-matches #".*Monkey (\d+).*" s)
        [_ items]
        (re-matches #".*Starting items: ([, \d]+).*" s)
        items
        (mapv #(Long/parseLong %) (str/split items #"[,\s]+"))
        [_ a operator b]
        (re-matches #".*Operation: new = (old|\d+) ([*+]) (old|\d+).*"  s)
        operator
        (case operator "*" * "+" +)
        divisor
        (Long/parseLong (second (re-matches #".*Test: divisible by (\d+).*"  s)))
        monkey-true
        (Long/parseLong (second (re-matches #".*If true: throw to monkey (\d+).*"  s)))
        monkey-false
        (Long/parseLong (second (re-matches #".*If false: throw to monkey (\d+).*"  s)))]
    {:id (Long/parseLong monkey-id)
     :items items
     :inspected 0
     :divisor divisor
     :operation (fn [old] (operator
                          (if (= a "old") old (Long/parseLong a))
                          (if (= b "old") old (Long/parseLong b))))
     :choose-monkey (fn [x] (if (divisible-by? x divisor)
                             monkey-true
                             monkey-false))}))

(defn group-by-monkey [lines]
  (->> lines
       (partition-by empty?)
       (remove #(empty? (first %)))))

(defn parse-monkeys [lines]
  (->> lines
       (group-by-monkey)
       (map parse-monkey)
       (map (juxt :id identity))
       (into {})))

;; -----------------------------------------------------------------------------

(defn turn [worry-fn monkeys id]
  (let [{:keys [items operation choose-monkey]} (get monkeys id)]
    (reduce (fn [monkeys item]
              (let [item-worry-level (operation item)
                    item-worry-level (worry-fn item-worry-level)
                    target-monkey (choose-monkey item-worry-level)]
                (-> monkeys
                    (update-in [id :inspected] inc)
                    (update-in [id :items] #(subvec % 1))
                    (update-in [target-monkey :items] conj item-worry-level))))
            monkeys
            items)))

(defn round [worry-fn monkeys]
  (reduce (partial turn worry-fn)
          monkeys
          (range (count monkeys))))

(defn most-active [monkeys n]
  (->> monkeys
       (sort-by (fn [[i monkey]] (:inspected monkey)) >)
       (take 2)
       (map second)))

(defn monkey-business
  "The level of monkey business after n rounds using the worry-fn to
  compute the new worry level of an item after each operation"
  [monkeys n-rounds worry-fn]
  (let [monkeys (parse-monkeys input)
        monkeys-after-n-rounds
        (nth (iterate (partial round worry-fn) monkeys) n-rounds)
        two-most-active (most-active monkeys-after-n-rounds 2)]
    (apply * (map :inspected two-most-active))))

(defn part-one
  "Level of monkey business after 20 rounds where the worry level after
  each operation is divided by 3"
  [input]
  (let [monkeys (parse-monkeys input)
        worry-fn #(int (Math/floor (/ % 3)))]
    (monkey-business monkeys 20 worry-fn)))

(defn part-two
  "Level of monkey business after 10000 rounds"
  [input]
  (let [monkeys (parse-monkeys input)
        N (reduce * (map :divisor (vals monkeys)))
        worry-fn #(mod % N)]
    (monkey-business  monkeys 10000 worry-fn)))

(comment
  (time (part-one input))
  (time (part-two input))
  )
