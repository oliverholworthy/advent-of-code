(ns advent-of-code.2019.12
  "Day 12: The N-Body Problem")

(def input "<x=-8, y=-9, z=-7>
  <x=-5, y=2, z=-1>
  <x=11, y=8, z=-14>
  <x=1, y=-4, z=-11>")

(defn parse-coord [s]
  (let [[_ x y z] (re-matches #"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>" s)]
    [(Long/parseLong x) (Long/parseLong y) (Long/parseLong z)]))

(defn parse-input [s]
  (map (comp parse-coord str/trim) (str/split-lines s)))

(defn gravity [p1 p2]
  [(mapv (fn [a b] (cond (> a b) -1 (< a b) 1 :else 0)) p1 p2)
   (mapv (fn [a b] (cond (> a b) 1 (< a b) -1 :else 0)) p1 p2)])

(defn v-op [op]
  (fn [v1 v2]
    (let [[x1 y1 z1] v1
          [x2 y2 z2] v2]
      [(op x1 x2) (op y1 y2) (op z1 z2)])))

(def v-add (v-op +))

(defn apply-gravity [moons]
  (loop [moons moons
         res []]
    (if-let [m1 (first moons)]
      (let [[m1 ms]
            (reduce (fn [[m1 ms] m2]
                      (let [[d1 d2] (gravity (:position m1) (:position m2))]
                        [(assoc m1 :velocity (v-add d1  (:velocity m1)))
                         (conj ms (assoc m2 :velocity (v-add d2 (:velocity m2))))]))
                    [m1 []]
                    (rest moons))]
        (recur ms
               (conj res m1)))
      res)))

(defn apply-velocity [moons]
  (mapv (fn [moon] (assoc moon :position (v-add (:velocity moon) (:position moon)))) moons))

(defn step [moons]
  (->> moons (apply-gravity) (apply-velocity)))

(defn total-energy [moon]
  (let [potential-energy (reduce + (map #(Math/abs %) (:position moon)))
        kinetic-energy (reduce + (map #(Math/abs %) (:velocity moon)))]
    (* potential-energy kinetic-energy)))

(defn gcd [a b]
  (bigint (.gcd (BigInteger. (str a)) (BigInteger. (str b)))))

(defn lcm [a b]
  (bigint (/ (* a b) (gcd a b))))

(defn moon-state [moons axis]
  (mapv (fn [moon] [(get-in moon [:velocity axis])
                   (get-in moon [:position axis])])
        moons))

(defn periods [moons]
  (let [start (reduce (fn [acc axis] (conj acc (moon-state moons axis)))
                      [] (range 3))]
    (loop [moons moons
           periods {}
           steps 0]
      (if (= (count periods) 3)
        {:periods periods :steps steps}
        (let [moons (step moons)
              steps (inc steps)]
          (recur moons
                 (reduce (fn [acc axis] (if (and (not (get acc axis))
                                                (= (get start axis) (moon-state moons axis)))
                                         (assoc acc axis steps)
                                         acc))
                         periods (range 3))
                 steps))))))

(comment
  ;; Part One
  (let [moons
        (mapv (fn [p] {:position p :velocity [0 0 0]}) (parse-input input))]
    (reduce + (map total-energy (nth (iterate step moons) 1000))))

  ;; Part Two
  (def res
    (let [moons
          (mapv (fn [p] {:position p :velocity [0 0 0]}) (parse-input input))]
      (reduce lcm (vals (:periods (periods moons))))))
  )
