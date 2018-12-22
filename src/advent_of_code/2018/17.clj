(ns advent-of-code.2018.17
  "Day 17: Reservoir Research"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-lines
  (str/split-lines
   (slurp (io/resource "2018/17/input.txt"))))

(def input-lines-sample
  (str/split-lines
   (slurp (io/resource "2018/17/input-sample.txt"))))

(defn get-sample-grid [n]
  (reduce (fn [g line]
            (conj g (mapv (fn [c] {:cell/type
                                  (case (str c)
                                    "+" :spring
                                    "." :sand
                                    "#" :clay)}) line)))
          []
          (str/split-lines
           (slurp (io/resource (str "2018/17/grid-sample-" n ".txt"))))))

(defn parse-input [lines]
  (let [x-ys-re #"x=(\d+), y=(\d+)..(\d+)"
        y-xs-re #"y=(\d+), x=(\d+)..(\d+)"]
    (reduce (fn [acc line]
              (cond (re-matches x-ys-re line)
                    (let [[_ x y1 y2] (re-find x-ys-re line)
                          [x y1 y2] (mapv #(Long/parseLong %) [x y1 y2])]
                      (conj acc {:x [x x] :y [y1 y2]}))

                    (re-matches y-xs-re line)
                    (let [[_ y x1 x2] (re-find y-xs-re line)
                          [y x1 x2] (mapv #(Long/parseLong %) [y x1 x2])]
                      (conj acc {:x [x1 x2] :y [y y]}))))
            []
            lines)))

;; -------------------------------------------------------------------------

(defn max-coords [coords]
  (reduce (fn [acc {:keys [x y]}]
            (let [update-fn (fn [vs f] (fn [v] (apply f (cond-> vs v (conj v)))))]
              (-> acc
                  (update :x-min (update-fn x min))
                  (update :x-max (update-fn x max))
                  (update :y-min (update-fn y min))
                  (update :y-max (update-fn y max)))))
          {}
          coords))

(defn get-grid [input-coords]
  (let [{:keys [x-min x-max y-min y-max]} (max-coords input-coords)]
    (vec (for [y (range 0 (inc y-max))]
           (vec (for [x (range (- x-min 5) (+ x-max 5))]
                  {:cell/type :sand}))))))

(defn coord-range [{:keys [x y]}]
  (let [[x1 x2] x [y1 y2] y]
    (for [y (range y1 (inc y2))
          x (range x1 (inc x2))]
      [y x])))

(defn setup-grid [grid input-coords]
  (let [{:keys [x-min]} (max-coords input-coords)]
    (reduce (fn [acc coord]
              (reduce (fn [g [y x]]
                        (assoc-in g (conj [y (- x (- x-min 5))]
                                          :cell/type)
                                  :clay))
                      acc
                      (coord-range coord)))
            grid
            input-coords)))

(defn print-grid [{:keys [grid pos dir]}]
  (println
   (str/join "\n"
             (for [[i row] (map-indexed vector grid)]
               (str/join (cond->
                             (for [cell row]
                               (case (:cell/type cell)
                                 :sand "."
                                 :clay "#"
                                 :spring "+"
                                 :water/falling "|"
                                 :water/at-rest "~"))
                           (= i (first pos))
                           (concat [" " (str pos) " " dir])))))))

(defn step [{:keys [grid pos dir path] :as acc}]
  (let [cell (get-in grid pos)
        [y x] pos
        cell-type (:cell/type cell)]
    (cond
      ;; SPRING to SAND (DOWN)
      (and (or (= :spring cell-type) (= :water/falling cell-type))
           (= :down dir)
           (= :sand (:cell/type (get-in grid [(inc y) x]))))
      (let [new-pos [(inc y) x]]
        (-> acc
            (update :grid (fn [g] (assoc-in g
                                           (conj new-pos :cell/type)
                                           :water/falling)))
            (update :pos (fn [_] new-pos))
            (update :path (fn [p] (conj p new-pos)))))

      ;; WATER/FALLING to CLAY (DOWN)
      (and (= :water/falling cell-type)
           (= :down dir)
           (or (= :clay (:cell/type (get-in grid [(inc y) x])))
               (= :water/at-rest (:cell/type (get-in grid [(inc y) x])))))
      (let [cell-type-below (:cell/type (get-in grid [(inc y) x]))
            new-cell-type :water/falling
            cell-type-right (:cell/type (get-in grid [y (inc x)]))
            cell-type-left (:cell/type (get-in grid [y (dec x)]))]
        (cond (= :sand (:cell/type (get-in grid [y (dec x)])))
              (let [new-pos [y (dec x)]]
                (-> acc
                    (update :grid (fn [g]
                                    (-> g
                                        (assoc-in (conj new-pos :cell/type)
                                                  new-cell-type))))
                    (update :pos (fn [_] new-pos))
                    (update :dir (fn [_] (if (= :sand (:cell/type (get-in grid [(inc y) (dec x)])))
                                          :down
                                          :left)))
                    (update :path (fn [p] (conj p new-pos)))))
              (= :sand (:cell/type (get-in grid [y (inc x)])))
              (let [new-pos [y (inc x)]]
                (-> acc
                    (update :grid (fn [g]
                                    (-> g
                                        (assoc-in (conj new-pos :cell/type)
                                                  new-cell-type))))
                    (update :pos (fn [_] new-pos))
                    (update :dir (fn [_] (if (= :sand (:cell/type (get-in grid [(inc y) (inc x)])))
                                          :down
                                          :right)))
                    (update :path (fn [p] (conj p new-pos)))))
              :else
              (let [new-path (pop path)
                    new-pos (last new-path)]
                (-> acc
                    (update :wall (fn [w] (if (< (first new-pos) (first pos))
                                           {}
                                           w)))
                    (update :grid
                            (fn [g] (cond-> g
                                     (< (first new-pos) (first pos))
                                     (fill-row pos))))
                    (update :pos (fn [_] new-pos))
                    (update :path (fn [_] new-path))))))

      ;; WATER/AT-REST to SAND (RIGHT)
      (and (or (= :water/at-rest cell-type)
               (= :water/falling cell-type))
           (or (and (= :right dir)
                    (= :sand (:cell/type (get-in grid [y (inc x)]))))
               (and (= :left dir)
                    (= :sand (:cell/type (get-in grid [y (dec x)]))))))
      (let [new-pos (if (= dir :right) [y (inc x)] [y (dec x)])
            [new-y new-x] new-pos
            new-cell-type :water/falling
            new-dir
            (if (= :sand (get-in grid [(inc new-y) new-x :cell/type]))
              :down
              dir)]
        (-> acc
            (update :grid (fn [g] (-> g
                                     (assoc-in (conj new-pos :cell/type)
                                               new-cell-type))))
            (update :pos (fn [_] new-pos))
            (update :dir (fn [_] new-dir))
            (update :path (fn [p] (conj p new-pos)))))

      ;; WATER/FALLING to CLAY (LEFT/RIGHT)
      (and (= :water/falling cell-type)
           (or (and (= :left dir)
                    (= :clay (:cell/type (get-in grid [y (dec x)]))))
               (and (= :right dir)
                    (= :clay (:cell/type (get-in grid [y (inc x)]))))))
      (-> acc
          (update :dir (fn [_] :down))
          (assoc-in [:wall dir] true))

      (and (= :water/falling cell-type)
           (or
            ;; at the bottom of the grid
            (= (first pos) (dec (count grid)))
            ;; or flowing horizontally into already falling watter
            (or (and (= :left dir)
                     (= :water/falling (:cell/type (get-in grid [y (dec x)]))))
                (and (= :right dir)
                     (= :water/falling (:cell/type (get-in grid [y (inc x)]))))
                (and (= :down dir)
                     (= :water/falling (:cell/type (get-in grid [(inc y) x])))))))
      (let [[i new-pos]
            (first
             (drop-while
              (fn [[i c]]
                (let [[y x] c]
                  (not
                   (and (or (= :sand
                               (get-in grid (conj [y (inc x)] :cell/type)))
                            (= :sand
                               (get-in grid (conj [y (dec x)] :cell/type))))
                        (contains? #{:clay :water/at-rest}
                                   (get-in grid (conj [(inc y) x] :cell/type)))
                        (= :water/falling
                           (get-in grid (conj c :cell/type)))))))
              (map-indexed vector (reverse path))))]
        (when i
          (let [new-path (vec (drop-last i path))]
            (-> acc
                (update :dir (fn [_] :down))
                (update :pos (fn [_] new-pos))
                (update :path (fn [_] new-path)))))))))

(defn init [input-coords]
  (let [grid (get-grid input-coords)
        grid (setup-grid grid input-coords)
        {:keys [x-min]} (max-coords input-coords)
        spring-pos [0 (- 500 (- x-min 5))]
        grid (assoc-in grid (conj spring-pos :cell/type) :spring)]
    {:grid grid :pos spring-pos :dir :down :path []}))

(defn fill-grid [initial-state]
  (loop [i 0
         acc initial-state]
    (if-let [next-acc (step acc)]
      (recur (inc i) next-acc)
      (assoc acc :steps i))))

(defn coords-dir [grid pos dir-fn]
  (loop [res []
         [y x] pos]
    (if (and (= :water/falling (:cell/type (get-in grid [y x])))
             (#{:clay :water/at-rest} (:cell/type (get-in grid [(inc y) x]))))
      (recur (conj res [y x])
             [y (dir-fn x)])
      (when (= :clay (:cell/type (get-in grid [y x])))
        res))))

(defn fill-row [grid [y x]]
  (let [coords-r (coords-dir grid [y (inc x)] inc)
        coords-l (coords-dir grid [y (dec x)] dec)]
    (if (and (not (nil? coords-r))
             (not (nil? coords-l)))
      (reduce (fn [g c]
                (assoc-in g (conj c :cell/type) :water/at-rest))
              grid
              (concat coords-l [[y x]] coords-r))
      grid)))

(defn cell-type-freqs [grid]
  (->> grid
       (flatten)
       (map :cell/type)
       (frequencies)))

(defn water-count [grid]
  (let [cell-freqs (cell-type-freqs grid)]
    (+ (:water/at-rest cell-freqs 0)
       (:water/falling cell-freqs 0))))

(defn part-one [input-coords]
  (let [{:keys [y-min]} (max-coords input-coords)]
    (- (water-count (:grid (fill-grid (init input-coords))))
       (dec y-min))))

(defn part-two [input-coords]
  (:water/at-rest (cell-type-freqs (:grid (fill-grid (init input-coords))))))

(comment
  (print-grid (fill-grid {:grid (get-sample-grid 2) :pos [0 10] :dir :down :path []}))
  (print-grid (fill-grid {:grid (get-sample-grid 3) :pos [0 10] :dir :down :path []}))
  (print-grid (fill-grid {:grid (get-sample-grid 4) :pos [0 10] :dir :down :path []}))
  (print-grid (fill-grid {:grid (get-sample-grid 5) :pos [0 10] :dir :down :path []}))
  (print-grid (fill-grid {:grid (get-sample-grid 6) :pos [0 10] :dir :down :path []}))
  (print-grid (fill-grid {:grid (get-sample-grid 7) :pos [0 10] :dir :down :path []}))
  (print-grid (fill-grid {:grid (get-sample-grid 8) :pos [0 10] :dir :down :path []}))

  (part-one (parse-input input-lines-sample))
  (part-two (parse-input input-lines-sample))

  (part-one (parse-input input-lines))
  (part-two (parse-input input-lines))
  )
