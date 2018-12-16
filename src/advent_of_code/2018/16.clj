(ns advent-of-code.2018.16
  "Day 16: Chronal Classification"
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input-lines-samples
  (str/split-lines
   (slurp (io/resource "2018/16/input-samples.txt"))))

(def input-lines-program
  (str/split-lines
   (slurp (io/resource "2018/16/input-program.txt"))))

(defn parse-samples [lines]
  (map (fn [[b i a]]
         {:before
          (mapv #(Long/parseLong %)
                (rest (re-find #"Before:\s*\[(\d), (\d), (\d), (\d)\]" b)))
          :instruction
          (mapv #(Long/parseLong %)
                (str/split i #"\s+"))
          :after
          (mapv #(Long/parseLong %)
                (rest (re-find #"After:\s*\[(\d), (\d), (\d), (\d)\]" a)))})
       (partition 3 (remove empty? lines))))

(defn parse-program [lines]
  (map (fn [line] (mapv #(Long/parseLong %) (str/split line #"\s+"))) lines))

(def opcodes
  [{:name "addr"
    :fn (fn [r [a b c]] (assoc r c (+ (get r a) (get r b))))}
   {:name "addi"
    :fn (fn [r [a b c]] (assoc r c (+ (get r a) b)))}
   {:name "mulr"
    :fn (fn [r [a b c]] (assoc r c (* (get r a) (get r b))))}
   {:name "muli"
    :fn (fn [r [a b c]] (assoc r c (* (get r a) b)))}
   {:name "borr"
    :fn (fn [r [a b c]] (assoc r c (bit-or (get r a) (get r b))))}
   {:name "bori"
    :fn (fn [r [a b c]] (assoc r c (bit-or (get r a) b)))}
   {:name "banr"
    :fn (fn [r [a b c]] (assoc r c (bit-and (get r a) (get r b))))}
   {:name "bani"
    :fn (fn [r [a b c]] (assoc r c (bit-and (get r a) b)))}
   {:name "setr"
    :fn (fn [r [a b c]] (assoc r c (get r a)))}
   {:name "seti"
    :fn (fn [r [a b c]] (assoc r c a))}
   {:name "gtir"
    :fn (fn [r [a b c]] (assoc r c (if (> a (get r b)) 1 0)))}
   {:name "gtri"
    :fn (fn [r [a b c]] (assoc r c (if (> (get r a) b) 1 0)))}
   {:name "gtrr"
    :fn (fn [r [a b c]] (assoc r c (if (> (get r a) (get r b)) 1 0)))}
   {:name "eqir"
    :fn (fn [r [a b c]] (assoc r c (if (= a (get r b)) 1 0)))}
   {:name "eqri"
    :fn (fn [r [a b c]] (assoc r c (if (= (get r a) b) 1 0)))}
   {:name "eqrr"
    :fn (fn [r [a b c]] (assoc r c (if (= (get r a) (get r b)) 1 0)))}])

(defn opcode-match [{:keys [before instruction after]}]
  (map :name
       (filter (fn [opcode]
                 (= ((:fn opcode) before (rest instruction))
                    after))
               opcodes)))

(defn part-one [samples]
  (count
   (filter #(>= % 3) (map (comp count opcode-match) samples))))

(defn find-mapping [m]
  (loop [mapping {}
         possible m]
    (let [single-ops (filter (comp #(= % 1) count second) possible)]
      (if (empty? single-ops)
        (merge mapping possible)
        (let [new-mapping (reduce (fn [acc [i ops]] (assoc acc i (first ops)))
                                  mapping single-ops)
              new-possible
              (reduce
               (fn [acc [s-i s-ops]]
                 (into {}
                       (map (fn [[i ops]] [i (disj ops (first s-ops))])
                            (dissoc acc s-i))))
               possible
               single-ops)]
          (recur new-mapping new-possible))))))

(defn find-opcode [samples]
  (find-mapping
   (reduce (fn [acc sample]
             (let [i (first (:instruction sample))
                   new-s
                   (set/intersection
                    (set (opcode-match sample))
                    (get acc i))]
               (assoc acc i new-s)))
           (into {} (map (fn [i] [i (set (map :name opcodes))]) (range 16)))
           samples)))

(defn part-two [program opcode-mapping]
  (let [instruction-fn (into {} (map (juxt :name :fn) opcodes))]
    (reduce (fn [acc instruction]
              (let [opcode-name (get opcode-mapping (first instruction))
                    f (get instruction-fn opcode-name)]
                (f acc (rest instruction))))
            [0 0 0 0]
            program)))

(comment
  (def samples (parse-samples input-lines-samples))

  (opcode-match
   {:before [3 2 1 1]
    :instruction [9 2 1 2]
    :after [3 2 2 1]})

  (part-one samples) ;; => 560

  (def opcode-mapping (find-opcode samples))
  (def program (parse-program input-lines-program))

  (part-two program opcode-mapping) ;; => [622 0 622 1]
  )
