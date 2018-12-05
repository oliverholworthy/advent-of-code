(ns advent-of-code.2018.04
  "Day 4: Repose Record"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/04/input.txt"))))

(def sample-input-lines
  (str/split-lines
   (slurp (io/resource "2018/04/sample.txt"))))

(defn parse-long [x] (Long/parseLong x))

(defn guard-begins-shift [msg]
  (let [guard-ptn #"Guard #(\d+) begins shift"]
    (when (re-matches guard-ptn msg)
      (parse-long (second (re-find guard-ptn msg))))))

(defn parse-message [s]
  (let [guard-ptn #"Guard #(\d+) begins shift"]
    (cond (= s "wakes up") :wakes-up
          (= s "falls asleep") :falls-asleep
          (re-matches guard-ptn s) [:begins-shift (second (re-find guard-ptn s))])))

(defn parse-line [line]
  (let [[_ y m d hh mm v]
        (re-find #"^\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (.*)$"
                 line)]
    {:line line
     :timestamp {:year (parse-long y)
                 :month (parse-long m)
                 :date (parse-long d)
                 :hour (parse-long hh)
                 :minute (parse-long mm)}
     :message v}))

(defn minutes-asleep [guard-msgs]
  (->> guard-msgs
       (partition 2)
       (mapcat (fn [[falls-asleep wakes-up]]
                 (range (:minute (:timestamp falls-asleep))
                        (:minute (:timestamp wakes-up)))))))

(defn groupby-guard [lines]
  (dissoc (reduce (fn [acc l]
                    (let [current-guard (:current-guard acc)]
                      (if-let [g (guard-begins-shift (:message l))]
                        (assoc acc :current-guard g)
                        (update acc current-guard
                                (fn [xs] (conj (or xs []) l))))))
                  {}
                  (->> lines
                       (sort)
                       (map parse-line)))
          :current-guard))

(defn part-one [lines]
  (->> lines
       (groupby-guard)
       (map (fn [[guard-id guard-msgs]]
              (let [mins-asleep (minutes-asleep guard-msgs)
                    min-freqs (sort-by second > (frequencies mins-asleep))]
                {:guard-id guard-id
                 :count (count mins-asleep)
                 :most-min (ffirst min-freqs)})))
       (sort-by :count >)
       (first)
       ((juxt :guard-id :most-min))
       (apply *)))

(comment
  (part-one input-lines)
  )

;; -------------------------------------------------------------------------


(defn part-two [lines]
  (->> lines
       (groupby-guard)
       (map (fn [[guard-id guard-msgs]]
              (let [mins-asleep (minutes-asleep guard-msgs)
                    min-freqs (sort-by second > (frequencies mins-asleep))]
                {:guard-id guard-id
                 :count (count mins-asleep)
                 :most-min (ffirst min-freqs)
                 :most-freq (second (first min-freqs))})))
       (sort-by :most-freq >)
       (first)
       ((juxt :guard-id :most-min))
       (apply *)))

(comment
  (part-two input-lines)
  )
