(ns advent-of-code.2018.z3
  (:import [com.microsoft.z3
            Context Version Context
            Expr ArithExpr BoolExpr
            Sort Status Native]))

(def ^:dynamic *context* nil)

(defn third [seqable] (nth seqable 2))

(defn ->string [thing]
  (cond
    (symbol? thing)  (name thing)
    (keyword? thing) (name thing)
    :else            (str thing)))

(defn interleaved->map [params]
  (into {} (map #(clojure.core/apply vector %) (partition 2 params))))

(defn Int 
  ([identifier-or-value]
   (if (string? identifier-or-value)
     (.mkIntConst *context* identifier-or-value)
     (.mkInt      *context* identifier-or-value)))
  ([identifier size]
   (.mkBVConst *context* identifier size)))

(defmacro with-context [params & body]
  (let [params (into [] (map ->string params))]
    `(let [new-context# (Context. ~(interleaved->map params))
           result#      (binding [*context* new-context#] ~@body)
           result#      (if (sequential? result#) (doall result#) result#)]
       (.close new-context#)
       result#)))

(defn get-sort [arg]
  (condp = arg
    'Int    (.getIntSort    *context*)
    'Real   (.getRealSort   *context*)
    'Bool   (.getBoolSort   *context*)
    'String (.getStringSort *context*)
    'unknown-sort))

(defn- make-domain [sorts]
  (into-array Sort 
              (if (sequential? sorts)
                (map get-sort sorts)
                [(get-sort sorts)])))

(defn make-func-decl [name domain range]
  (.mkFuncDecl *context* name (make-domain domain) (get-sort range)))

(defn make-decl [name sort]
  (if (sequential? sort)
    (condp = (first sort)
      'Func (make-func-decl name (second sort) (third sort)))
    (do
      (.mkConst *context* name (get-sort sort)))))

(defn flat-vec [arg]
  (into [] (reduce concat arg)))

(defmacro with-decls [decls & body]
  (let [decls
        (flat-vec (for [[sort name] (partition 2 decls)]
                    [name `(make-decl ~(->string name) '~sort)] ))]
    `(let ~decls ~@body)))

(defn optimizer [ & constraints]
  (let [opt (.mkOptimize *context*)]
    (.Add opt (into-array BoolExpr constraints))
    opt))

(defn EQ [lhs rhs]
  (.mkEq *context* lhs rhs))

(defn LT [lhs rhs]
  (.mkLt *context* lhs rhs))

(defn LE [lhs rhs]
  (.mkLe *context* lhs rhs))

(defn GT [lhs rhs]
  (.mkGt *context* lhs rhs))

(defn GE [lhs rhs]
  (.mkGe *context* lhs rhs))

(defn IFF [lhs rhs]
  (.mkIff *context* lhs rhs))

(defn ITE [bool-expr lhs rhs]
  (.mkITE *context* bool-expr lhs rhs))

(defn Plus [& arithmetic-expressions]
  (.mkAdd *context* (into-array ArithExpr arithmetic-expressions)))

(defn Minus [& args]
  (if (= 1 (count args))
    (.mkUnaryMinus *context* (first args))
    (.mkSub *context* (into-array ArithExpr args))))

(defn Divide [lhs rhs]
  (.mkDiv *context* lhs rhs))

(defn Real [identifier-or-value]
  (if (string? identifier-or-value)
    (.mkRealConst *context* identifier-or-value)
    (.mkReal      *context* identifier-or-value)))

(defn maximize [optimizer arithmetic-expression]
  (.MkMaximize optimizer arithmetic-expression))

(defn minimize [optimizer arithmetic-expression]
  (.MkMinimize optimizer arithmetic-expression))

(defn zabs [x]
  (ITE (GE x (Int 0)) x (Minus x)))

(comment
  (with-context [:model :true]
    (let [x (Int "x")
          y (Int "y")
          opt (optimizer (EQ (Minus x y) (Real 10))
                         (GT x (Real 100))
                         (LT y (Real 100))
                         (GE x (Real 0))
                         (GE y (Real 0)))
          mx (maximize opt (Divide x (Real 10)))
          my (maximize opt y)
          status (.Check opt nil)]
      mx
      ))
  )


