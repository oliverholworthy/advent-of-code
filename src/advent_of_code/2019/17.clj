(ns advent-of-code.2019.17
  "Day 17: Set and Forget"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [advent-of-code.2019.05 :refer [run init-state]]))

(def program-instructions (str/split (str/trim (slurp (io/resource "2019/17/input.txt"))) #","))

(defn get-map [mem]
  (mapv (fn [row] (mapv str row))
        (str/split (str/join
                    (mapv (comp char int)
                          (:outputs (run (init-state
                                          (concat mem (repeat 10000 "0"))
                                          [])))))
                   #"\n")))

(defn get-neighbours [grid [y x]]
  (reduce (fn [acc coord]
            (let [node (get-in grid coord)]
              (conj acc [coord node])))
          []
          [[y (dec x)]
           [(dec y) x]
           [(inc y) x]
           [y (inc x)]]))

(defn grid-coords [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))]
    [y x]))

(defn intersection-coords [grid]
  (reduce (fn [acc coord]
            (if (and (= "#" (get-in grid coord))
                     (every? (fn [[_ n]] (= n "#"))
                             (get-neighbours grid coord)))
              (conj acc coord)
              acc))
          []
          (grid-coords grid)))

(defn print-grid [grid]
  (println (str/join "\n" (map str/join grid))))

(defn print-intersections []
  (let [grid (get-map program-instructions)
        coords (part-one grid)]
    (print-grid
     (reduce (fn [acc c] (assoc-in acc c "O"))
             grid
             coords))))

(defn robot-coords [grid]
  (first
   (filter
    (comp #{"^" "<" ">" "v"} second)
    (map (juxt identity (partial get-in grid))
         (grid-coords grid)))))

(defn v-op [op]
  (fn [v1 v2]
    (let [[x1 y1] v1
          [x2 y2] v2]
      [(op x1 x2) (op y1 y2)])))

(def v-add (v-op +))
(def v-sub (v-op -))

(def robot-dir {"^" [-1 0] "<" [0 -1] ">" [0 1] "v" [1 0]})
(defn turn-left [[y x]] [(* -1 x) y])
(defn turn-right [[y x]] [x (* -1 y)])

(defn visit-every-path [grid]
  (let [[coord robot] (robot-coords grid)
        dir (get robot-dir robot)]
    (loop [coord coord
           dir dir
           path []]
      (if-let [next-dir
            (first (filter (fn [v] (= "#" (get-in grid (v-add v coord))))
                           [dir (turn-left dir) (turn-right dir)]))]
        (let [next-coord (v-add next-dir coord)]
          (recur next-coord next-dir (conj path next-coord)))
        path))))

(defn path-to-commands [path turn]
  (:commands
   (reduce (fn [{:keys [dirs prev-dir] :as acc} dir]
             (if (or (nil? prev-dir)
                     (= prev-dir dir))
               (assoc acc :prev-dir dir :dirs (conj dirs dir))
               (assoc acc
                      :prev-dir dir
                      :dirs []
                      :turn (if (= (turn-right prev-dir) dir) "R" "L")
                      :commands
                      (conj (:commands acc)
                            (str (:turn acc) (inc (count dirs)))))))
           {:commands []
            :turn turn
            :dirs []
            :prev-dir nil}
           (map (fn [[v1 v2]] (v-sub v2 v1)) (partition 2 1 path)))))

(def commands
  ["L10" "R8" "R8"          ;; A
   "L10" "R8" "R8"          ;; A
   "L10" "L12" "R8" "R10"   ;; B
   "R10" "L12" "R10"        ;; C
   "L10" "L12" "R8" "R10"   ;; B
   "R10" "L12" "R10"        ;; C
   "L10" "L12" "R8" "R10"   ;; B
   "R10" "L12" "R10"        ;; C
   "R10" "L12" "R10"        ;; C
   "L10" "R8"               ;; A
   ])

(def A "L,10,R,8,R,8")
(def B "L,10,L,12,R,8,R,10")
(def C "R,10,L,12,R,10")
(def main-routine "A,A,B,C,B,C,B,C,C,A")

(def part-two-input
  (->> [main-routine A B C "n"]
       (mapcat (comp #(conj % 10) #(mapv int %)))))

(defn part-two [mem inputs]
  (:outputs (run (init-state (concat mem (repeat 10000 "0")) inputs))))

(comment
  ;; Part One
  (print-intersections)
  (let [grid (get-map program-instructions)
        coords (part-one grid)]
    (reduce + (map (fn [c] (apply * c)) coords)))
  ;; => 6212

  (let [grid (get-map program-instructions)
        path (visit-every-path grid)
        starting-turn "L"
        commands (path-to-commands path starting-turn)]
    [commands
     (frequencies (partition 4 1 commands))])

  ;; Part Two
  (last (part-two (assoc program-instructions 0 "2")
                  part-two-input))
  ;; => 1016741N
  )
