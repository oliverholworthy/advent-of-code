(ns advent-of-code.2017.25
  "The Halting Problem")

(def blueprint
  {:A {0 {:write 1 :move right :state :B}
       1 {:write 0 :move left  :state :F}}
   :B {0 {:write 0 :move right :state :C}
       1 {:write 0 :move right :state :D}}
   :C {0 {:write 1 :move left  :state :D}
       1 {:write 1 :move right :state :E}}
   :D {0 {:write 0 :move left  :state :E}
       1 {:write 0 :move left  :state :D}}
   :E {0 {:write 0 :move right :state :A}
       1 {:write 1 :move right :state :C}}
   :F {0 {:write 1 :move left  :state :A}
       1 {:write 1 :move right :state :A}}})

(def left dec)
(def right inc)

(defn step [{:keys [tape state pos]}]
  (let [current-value (get tape pos 0)]
    (let [op (get-in blueprint [state current-value])]
      {:tape (assoc tape pos (:write op))
       :state (:state op)
       :pos ((:move op) pos)})))

(defn diagnostic-checksum [tape]
  (reduce + (vals tape)))

;; -----------------------------------------------------------------------------

(comment

  (def res
    (diagnostic-checksum
     (:tape (reduce (fn [acc i]
                      (step acc))
                    {:tape {} :state :A :pos 0}
                    (range 12794428)))))
  res
  ;; => 2832
  )
