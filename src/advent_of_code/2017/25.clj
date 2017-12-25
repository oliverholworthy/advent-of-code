(ns advent-of-code.2017.25
  "The Halting Problem")


(defn diagnostic-checksum [tape]
  (reduce + (vals tape)))

(def left dec)
(def right inc)

(defn step [{:keys [tape state pos]}]
  (let [current-value (get tape pos 0)]
    (case state
      :A (if (= current-value 0)
           {:tape (assoc tape pos 1)
            :pos (right pos)
            :state :B}
           {:tape (assoc tape pos 0)
            :pos (left pos)
            :state :F})
      :B (if (= current-value 0)
           {:tape (assoc tape pos 0)
            :pos (right pos)
            :state :C}
           {:tape (assoc tape pos 0)
            :pos (right pos)
            :state :D})
      :C (if (= current-value 0)
           {:tape (assoc tape pos 1)
            :pos (left pos)
            :state :D}
           {:tape (assoc tape pos 1)
            :pos (right pos)
            :state :E})
      :D (if (= current-value 0)
           {:tape (assoc tape pos 0)
            :pos (left pos)
            :state :E}
           {:tape (assoc tape pos 0)
            :pos (left pos)
            :state :D})
      :E (if (= current-value 0)
           {:tape (assoc tape pos 0)
            :pos (right pos)
            :state :A}
           {:tape (assoc tape pos 1)
            :pos (right pos)
            :state :C})
      :F (if (= current-value 0)
           {:tape (assoc tape pos 1)
            :pos (left pos)
            :state :A}
           {:tape (assoc tape pos 1)
            :pos (right pos)
            :state :A}))))

;; -----------------------------------------------------------------------------

(comment

  (def res
    (diagnostic-checksum
     (:tape (reduce (fn [acc i]
                      (step acc))
                    {:tape {} :state :A :pos 0}
                    (range 12794428)))))
  ;; => 2832
  )
