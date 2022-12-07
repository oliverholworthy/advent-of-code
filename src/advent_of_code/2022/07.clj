(ns advent-of-code.2022.07
  "Day 7: No Space Left On Device"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.walk :refer [prewalk]]))

(def input (str/split-lines (slurp (io/resource "2022/07.txt"))))
(def input-sample (str/split-lines (slurp (io/resource "2022/07.sample.txt"))))

(defn parse-command [s]
  (let [[_ command arg] (re-matches #"\$ (\w+) ?(.*)?" s)]
    {:command command
     :argument (if (not (empty? arg)) arg)}))

(defn command? [s] (= \$ (first s)))

(defn group-commands [input]
  "group command and output together from history"
  (reduce (fn [command-output line]
            (if (command? line)
              (conj command-output (parse-command line))
              (update-in command-output
                         [(dec (count command-output)) :output]
                         (fn [output] (conj (or output []) line)))))
          []
          input))

(defn parse-ls-output [output]
  (reduce (fn [file-sizes s]
            (if (str/starts-with? s "dir")
              file-sizes
              (let [[file-size filename] (str/split s #"\s+")
                    file-size (Long/parseLong file-size)]
                (assoc file-sizes filename file-size))))
          {}
          output))

(defn update-state [{:keys [working-directory] :as directory-state}
                    {:keys [command argument output]}]
  "keep track of directory state and file sizes after running one command"
  (case command
    "cd" (update directory-state
                 :working-directory
                 (fn [current-working-directory]
                   (cond (nil? current-working-directory) [argument]
                         (= ".." argument) (pop current-working-directory)
                         :else (conj current-working-directory argument))))
    "ls"
    (let [path (vec (interleave
                              (repeat (count working-directory) :directories)
                              working-directory))]
      (-> directory-state
          (assoc-in (conj path :files)
                    (parse-ls-output output))
          (assoc-in (conj path :path)
                    (str "/" (str/join "/" (rest working-directory))))))))

(def directory-size-total
  "total size of directory"
  (memoize (fn [{:keys [files directories]}]
             (+ (reduce + (vals files))
                (reduce + (map directory-size-total (vals directories)))))))

(defn parse-directory-state [input]
  (reduce update-state {} (group-commands input)))

(defn directory-sizes [directory-state]
  "returns sequence of dictionary name and total size"
  (->> directory-state
       (tree-seq map? (fn [node] (vals (:directories node))))
       (rest)
       (map (juxt :path directory-size-total))))

(defn part-one [input]
  "Sum of directory total sizes that have a size of at most 100000"
  (->> (parse-directory-state input)
       (directory-sizes)
       (filter (fn [[path size]] (<= size 100000)))
       (map second)
       (reduce +)))

(defn part-two [input]
  "Smallest directory that, if deleted, would free up enough space."
  (let [directory-state (parse-directory-state input)
        disk-space 70000000
        required-free-space 30000000
        used-space (directory-size-total directory-state)
        unused-space (- disk-space used-space)
        size-to-delete (- required-free-space unused-space)]
    (->> directory-state
         (directory-sizes)
         (filter (fn [[path size]] (>= size size-to-delete)))
         (sort-by second)
         (first))))

(comment
  (part-one input-sample) ;; =>95437
  (part-one input) ;; => 1449447

  (part-two input-sample) ;; => ["/d" 24933642]
  (part-two input) ;; => ["/jbt/bbm/tvqh/vjdjl" 8679207]
)
