(ns advent-of-code.2021.16
  "Day 16: Packet Decoder"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (str/trim (slurp (io/resource "2021/16.txt"))))

(defn hex-to-binary [s]
  (str/replace
   (format "%4s" (Integer/toString (Integer/parseInt s 16) 2))
   " "
   "0"))

(defn decode-hex [s] (str/join (map (comp hex-to-binary str) s)))
(defn decode-binary [s] (Long/parseLong s 2))
(defn packet-version [packet] (decode-binary (subs packet 0 3)))
(defn packet-type [packet] (decode-binary (subs packet 3 6)))

(defn decode-packet-literal [packet]
  (let [[value length]
        (reduce (fn [{:keys [i parsed]} chars]
            (let [new-acc (concat parsed (rest chars))]
              (if (= (first chars) \1)
                {:parsed new-acc :i (+ i 5)}
                (reduced
                 [(decode-binary (str/join new-acc)) (+ i 5)]))))
          {:parsed '() :i 0}
          (partition-all 5 (subs packet 6)))]
    {:type :literal
     :packet-type (packet-type packet)
     :value value
     :version (packet-version packet)
     :length (+ 6 length)}))

(declare decode-packet)

(defn decode-packet-count [packet]
  (let [number-of-packets (decode-binary (subs packet 7 (+ 7 11)))
        {:keys [sub-packets sub-packet-length]}
        (reduce (fn [{:keys [sub-packets sub-packet-length]} _]
                  (let [sub-packet (decode-packet (subs packet (+ sub-packet-length 7 11)))]
                    {:sub-packets (conj sub-packets sub-packet)
                     :sub-packet-length (+ sub-packet-length (:length sub-packet))}))
                {:sub-packets []
                 :sub-packet-length 0}
                (range number-of-packets))]
    {:type :operator
     :packet-type (packet-type packet)
     :version (packet-version packet)
     :sub-packets sub-packets
     :length (+ 7 11 sub-packet-length)}))

(defn decode-packet-length [packet]
  (let [sub-packet-length
        (decode-binary (subs packet 7 (+ 7 15)))
        sub-packet-string
        (subs packet (+ 7 15) (+ 7 15 sub-packet-length))]
    {:type :operator
     :packet-type (packet-type packet)
     :version (packet-version packet)
     :length (+ 7 15 sub-packet-length)
     :sub-packets
     (loop [sub-packet-string sub-packet-string
            sub-packets []]
       (let [packet (decode-packet sub-packet-string)]
         (if (nil? packet)
           sub-packets
           (recur (subs sub-packet-string (:length packet))
                  (conj sub-packets packet)))))}))

(defn decode-packet [packet]
  (when (> (count packet) 7)
    (if (= (packet-type packet) 4)
      (decode-packet-literal packet)
      (let [length-type-id (subs packet 6 7)]
        (if (= length-type-id "0")
          (decode-packet-length packet)
          (decode-packet-count packet))))))

(defn version-sum [packet]
  (reduce + (cons (:version packet) (map version-sum (:sub-packets packet [])))))

(defn eval-packet [packet]
  (if (= (:type packet) :literal)
    (:value packet)
    (let [sub-packets (:sub-packets packet (list))
          operator-fn
          (case (:packet-type packet)
            0 +
            1 *
            2 min
            3 max
            5 (comp {false 0 true 1} >)
            6 (comp {false 0 true 1} <)
            7 (comp {false 0 true 1} =))]
      (reduce operator-fn (map eval-packet sub-packets)))))

(comment
  ;; Part One
  (version-sum (decode-packet (decode-hex "8A004A801A8002F478")))
  (version-sum (decode-packet (decode-hex "620080001611562C8802118E34")))
  (version-sum (decode-packet (decode-hex "C0015000016115A2E0802F182340")))
  (version-sum (decode-packet (decode-hex "A0016C880162017C3686B18A3D4780")))
  (version-sum (decode-packet (decode-hex input)))
  ;; Part Two
  (eval-packet (decode-packet (decode-hex "C200B40A82")))
  (eval-packet (decode-packet (decode-hex "04005AC33890")))
  (eval-packet (decode-packet (decode-hex "880086C3E88112")))
  (eval-packet (decode-packet (decode-hex "CE00C43D881120")))
  (eval-packet (decode-packet (decode-hex "D8005AC2A8F0")))
  (eval-packet (decode-packet (decode-hex "F600BC2D8F")))
  (eval-packet (decode-packet (decode-hex "9C005AC2F8F0")))
  (eval-packet (decode-packet (decode-hex "9C0141080250320F1802104A08")))
  (eval-packet (decode-packet (decode-hex input)))
  )
