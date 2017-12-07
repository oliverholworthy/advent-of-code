(ns advent-of-code.2017.3)

;; Part One

(defn ring-sizes [] (remove even? (range)))

(defn manhattan [[x y]] (+ (Math/abs x) (Math/abs y)))

(def dirs
  {3 [0 1] ;; N
   2 [-1 0] ;; W
   1 [0 -1] ;; S
   0 [1 0]}) ;; E

(defn apply-rotation-90 [[x y]]
  [(+ (* 0 x) (* 1 y))
   (+ (* -1 x) (* 0 y))])

(defn apply-vec [f [x1 y1] [x2 y2]]
  [(f x1 x2) (f y1 y2)])

(defn grid-coords [n]
  (if (<= n 1) [0 0]
      (let [[i ring-width]
            (first (drop-while
                    (fn [[i x]] (> n (* x x)))
                    (map-indexed vector (ring-sizes))))
            spiral-area (* ring-width ring-width)
            dir (get dirs (quot (- spiral-area n) (dec ring-width)))
            dir2 (apply-rotation-90 dir)
            v (- (/ (dec ring-width) 2)
                 (mod (- spiral-area n) (dec ring-width)))]
        (apply-vec +
                   (mapv #(* % i) dir2)
                   (mapv #(* % v) dir)))))

(def spiral-steps (comp manhattan grid-coords))

(defn test-spiral-steps []
  (assert (= (spiral-steps 1) 0))
  (assert (= (spiral-steps 12) 3))
  (assert (= (spiral-steps 23) 2))
  (assert (= (spiral-steps 1024) 31)))


;; Part Two

(defn neighbours [[x y]]
  [[(- x 1) (+ y 1)]
   [(- x 1) y]
   [(- x 1) (- y 1)]
   [(+ x 1) (- y 1)]
   [(+ x 1) y]
   [(+ x 1) (+ y 1)]
   [x (+ y 1)]
   [x (- y 1)]])

(defn spiral-sum [n]
  (loop [spiral {[0 0] 1}
         spiral-seq [1]
         i 2]
    (if (> i n)
      spiral-seq
      (let [coords (grid-coords i)
            val (apply + (remove nil? (map #(get spiral %) (neighbours coords))))]
        (recur (assoc spiral coords val)
               (conj spiral-seq val)
               (inc i))))))

;; (first (drop-while (fn [x] (< x 361527)) (spiral-sum 100)))
;; => 363010
