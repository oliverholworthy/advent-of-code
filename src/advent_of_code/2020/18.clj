(ns advent-of-code.2020.18
  "Day 18: Operation Order"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (str/split-lines
   (slurp (io/resource "2020/18/input.txt"))))

(defn rpn [tokens precedence]
  "Convert an infix notation expression to reverse polish notation
  using the shunting-yard algorithm"
  (loop [tokens tokens
         output clojure.lang.PersistentQueue/EMPTY
         operators '()]
    (if (empty? tokens)
      (loop [output output
             operators operators]
        (if (empty? operators)
          (for [x output] x)
          (recur (conj output (peek operators)) (pop operators))))
      (let [token (str (first tokens))
            {:keys [operators output]}
            (cond
              (re-matches #"\d" token)
              {:output (conj output (Long/parseLong token))
               :operators operators}
              (#{"+" "*"} token)
              (loop [output output
                     operators operators]
                (if (and (seq operators)
                         (not= "(" (peek operators))
                         (>= (precedence (peek operators))
                             (precedence (symbol token))))
                  (recur (conj output (peek operators))
                         (pop operators))
                  {:output output :operators
                   (conj operators (symbol token))}))
              (= "(" token)
              {:output output :operators (conj operators token)}
              (= ")" token)
              (loop [output output
                     operators operators]
                (if (and (not= "(" (peek operators)) (seq operators))
                  (recur (conj output (peek operators))
                         (pop operators))
                  (cond->
                      {:output output :operators operators}
                      (= "(" (peek operators))
                      (update :operators pop))))
              :else {:output output :operators operators})]
        (recur (rest tokens) output operators)))))

(defn to-expr [rpn]
  "Convert RPN to a Clojure exprression that can be evaluated"
  (first (reduce (fn [acc x]
                   (if (number? x)
                     (conj acc x)
                     (conj (drop 2 acc) (cons x (take 2 acc)))))
                 (list)
                 rpn)))

(defn part-one [input]
  "Evaluate with equal precedence of operators"
  (eval (to-expr (rpn input {'* 1 '+ 1}))))

(defn part-two [input]
  "Evaluate with higher precedence on addition"
  (eval (to-expr (rpn input {'* 1 '+ 2}))))

(comment
  (-> "4 * 9 + 9 * (3 + 5)" ;; infix
      (rpn {'* 1 '+ 2})     ;; RPN  (4 9 * 9 + 3 5 + *)
      (to-expr)             ;; clj  (* (+ 5 3) (* (+ 9 9) 4))
      (eval))               ;; =>   576

  ;; Part One
  (reduce + (map part-one input))

  ;; Part Two
  (reduce + (map part-two input))
  )
