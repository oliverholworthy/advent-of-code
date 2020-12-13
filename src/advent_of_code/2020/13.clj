(ns advent-of-code.2020.13
  "Day 13: Shuttle Search"
  (:require [clojure.string :as str]))

(defn parse-bus-ids [bus-ids-str]
  (map #(Long/parseLong %) (remove #(= % "x") (str/split bus-ids-str #","))))

(defn first-bus [earliest-time bus-ids-str]
  (let [bus-ids (parse-bus-ids bus-ids-str)]
    (loop [ts (drop  earliest-time (range))]
      (let [t (first ts)
            bus-arrivals (filter #(zero? (mod t %)) bus-ids)]
        (if (seq bus-arrivals)
          {:bus (first bus-arrivals) :time t}
          (recur (rest ts)))))))

(defn part-one [earliest-time bus-ids-str]
  (let [{:keys [bus time]} (first-bus earliest-time bus-ids-str)]
    (when (and bus time)
      (* (- time earliest-time) bus))))

(defn bus-indexes [bus-ids-str]
  (for [[i v] (map-indexed vector (str/split bus-ids-str #","))
        :when (not= v "x")]
    [i (Long/parseLong v)]))

(defn to-eqn [bus-ids-str]
  (map (fn [[i v]] (format "(t + %d) mod %d = 0" i v)) (bus-indexes bus-ids-str)))

(defn mul-inv [a b]
  (if (= 1 b)
    1
    (let [b0 b]
      (loop [[a b] [a b]
             [x0 x1] [0 1]]
        (if (<= a 1)
          (if (< x1 0) (+ x1 b0) x1)
          (recur [b (mod a b)] [(- x1 (* (quot a b) x0)) x0]))))))

(defn chinese-remainder
  [n a]
  (let [prod (apply * n)
        sum (reduce
             (fn [sum [n-i a-i]]
               (let [p (quot prod n-i)
                     inv-p (mul-inv p n-i)]
                 (+ sum (* a-i inv-p p))))
             0
             (map vector n a))]
    (mod sum prod)))

(comment
  (part-one 939 "7,13,x,x,59,x,31,19")
  (part-one 1002576
            "13,x,x,x,x,x,x,37,x,x,x,x,x,449,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,773,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17")

  ;; Part Two
  (let [bus-idx (bus-indexes "13,x,x,x,x,x,x,37,x,x,x,x,x,449,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,773,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17")
        N (apply * (map second bus-idx))]
    (mod (reduce + (map (fn [[r m]]
                          (* (quot (* (- m r) N) m)
                             (mul-inv (quot N m) m))) bus-idx))
         N))

  (let [bus-idx (bus-indexes "13,x,x,x,x,x,x,37,x,x,x,x,x,449,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,773,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17")
        n (mapv second bus-idx)
        a (mapv (fn [[i bus-id]] (- bus-id i)) bus-idx)]
    (chinese-remainder n a))

  ;; Wolfram Alpha (check value)
  (str/join ", " (to-eqn "13,x,x,x,x,x,x,37,x,x,x,x,x,449,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,773,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17"))
  )
