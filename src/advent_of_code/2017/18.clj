(ns advent-of-code.2017.18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.async :as async]))

(defn read-input [resource]
  (->> (slurp resource)
       (str/trim)
       (str/split-lines)
       (map (fn [line]
              (let [[f a b] (str/split line #"\s+")]
                (case f
                  "snd" [:snd {:register a}]
                  "set" [:set {:register a
                               :value b}]
                  "mul" [:mul {:register a
                               :value b}]
                  "add" [:add {:register a
                               :value b}]
                  "mod" [:mod {:register a
                               :value b}]
                  "rcv" [:rcv {:register a}]
                  "jgz" [:jgz {:register a
                               :value b}]))))))

(defn apply-instructions [operations]
  (let [n (count operations)]
    (loop [i 0
           registers {:rcv []}]
      (if (or (first (:rcv registers)) (or (>= i n) (< i 0)))
        registers
        (let [[operation {:keys [register value]}] (nth operations i)
              x (get registers register 0)
              y (when value (if (re-matches #"\-?\d+" value)
                              (Long/parseLong value)
                              (get registers value 0)))]
          (recur (if (and (= operation :jgz) (> x 0))
                   (+ i y)
                   (inc i))
                 (case operation
                   :snd (assoc registers :snd x)
                   :rcv (cond-> registers
                          (not (zero? x))
                          (update :rcv conj (:snd registers)))
                   :set (assoc registers register y)
                   :add (assoc registers register (+ x y))
                   :mul (assoc registers register (* x y))
                   :mod (assoc registers register (mod x y))
                   registers)))))))

(defn program [limit registers operations rcv-chan snd-chan]
  (let [n (count operations)]
    (loop [j 0
           i 0]
      (if (or (> j limit) ;; (first (:rcv registers))
              (or (>= i n) (< i 0)))
        [j i n @registers]
        (let [[operation {:keys [register value]}] (nth operations i)
              x (get @registers register 0)
              y (when value (if (re-matches #"\-?\d+" value)
                              (Long/parseLong value)
                              (get @registers value 0)))]
          (case operation
            :snd (do (async/>!! snd-chan x)
                     (swap! registers update :snd-count inc))
            :rcv
            (do
              (swap! registers assoc :rcv true)
              (swap! registers assoc register (async/<!! rcv-chan))
              (swap! registers update :rcv-count inc)
              (swap! registers assoc :rcv false))
            :set (swap! registers assoc register y)
            :add (swap! registers assoc register (+ x y))
            :mul (swap! registers assoc register (* x y))
            :mod (swap! registers assoc register (mod x y))
            nil)
          (recur (inc j)
                 (if (and (= operation :jgz) (> x 0))
                   (+ i y)
                   (inc i))))))))

(comment
  ;; Part One
  (apply-instructions (read-input (io/resource "2017/18/input.txt")))

  (def programs
    (let [operations (read-input (io/resource "2017/18/input.txt"))
          chan-0 (async/chan (async/buffer 1e6))
          chan-1 (async/chan (async/buffer 1e6))
          limit 1e7
          p0-state (atom {"p" 0 :snd-count 0 :rcv-count 0})
          p1-state (atom {"p" 1 :snd-count 0 :rcv-count 0})
          p0 (future (try (program limit p0-state
                                   operations chan-0 chan-1)
                          (catch Exception e :error)))
          p1 (future (try (program limit p1-state
                                   operations chan-1 chan-0)
                          (catch Exception e
                            (println e)
                            :error)))]
      {:p0 p0
       :p1 p1
       :p0-state p0-state
       :p1-state p1-state}))

  @(:p0-state programs)
  @(:p1-state programs)
  (:p0 programs)
  (:p1 programs)


  (doseq [p programs] (future-cancel p))
  )
