(ns cljcc.tacky
  (:require
   [clojure.pprint :as pp]
   [cljcc.lexer :as l]
   [cljcc.parser :as p]))

(def counter "Global integer counter for generating unique identifier names." (atom 0))

(defn create-identifier
  "Returns a unique identifier. Used for generating tacky variable names."
  ([]
   (create-identifier "tmp"))
  ([identifier]
   (let [n @counter
         _ (swap! counter #(+ % 1))]
     (str identifier "." n))))

(defn variable
  ([]
   {:type :variable
    :value (create-identifier)})
  ([^String identifier]
   {:type :variable
    :value (create-identifier identifier)}))

(defn constant-instruction [^Integer v]
  {:type :constant
   :value v})

(defn- unary-operator [^String unop]
  (condp = unop
    "~" :complement
    "-" :negate))

(defn- binary-operator [binop]
  (condp = binop
    :add-exp :add
    :sub-exp :sub
    :mul-exp :mul
    :div-exp :div
    :mod-exp :mod
    :bit-and-exp :bit-and
    :bit-or-exp :bit-or
    :bit-xor-exp :bit-xor
    :bit-right-shift-exp :bit-right-shift
    :bit-left-shift-exp :bit-left-shift))

(defn- unary-instruction [unary-operator src dst]
  {:type :unary
   :unary-operator unary-operator
   :dst dst
   :src src})

(defn- binary-instruction [binary-operator src1 src2 dst]
  {:type :binary
   :binary-operator binary-operator
   :src1 src1
   :src2 src2
   :dst dst})

(defn return-instruction [val]
  {:type :return
   :val val})

(def binary-exprs
  #{:add-exp
    :sub-exp
    :mul-exp
    :mod-exp
    :div-exp
    :bit-and-exp
    :bit-or-exp
    :bit-xor-exp
    :bit-right-shift-exp
    :bit-left-shift-exp})

(defn- binary-expr? [v]
  (contains? binary-exprs v))

(declare expression-handler)

(defn- constant-expr-handler [e]
  {:val (constant-instruction (:value e))})

(defn- unary-expr-handler [e]
  (let [inner (expression-handler (nth e 2))
        dst (variable)
        src (:val inner)
        unary-operator (unary-operator (second (second e)))
        instruction (unary-instruction unary-operator src dst)]
    {:val dst
     :instructions (flatten [(:instructions inner) instruction])}))

(defn- binary-expr-handler [e]
  (let [e1 (expression-handler (nth e 1))
        e2 (expression-handler (nth e 2))
        src1 (:val e1)
        src2 (:val e2)
        dst (variable)
        binary-operator (binary-operator (first e))
        instruction (binary-instruction binary-operator src1 src2 dst)]
    {:val dst
     :instructions (flatten [(:instructions e1) (:instructions e2) instruction])}))

(defn- expression-handler [e]
  (when-let [exp-type (:type e)]
    (cond
      (= exp-type :constant-exp) (constant-expr-handler e)
      (= exp-type :unary-exp) (unary-expr-handler e)
      (binary-expr? exp-type) (binary-expr-handler e))))

(defn- exp-instructions [exp]
  (expression-handler (:value exp)))

(defn- ret-instructions [exp]
  (let [e (exp-instructions exp)
        val (:val e)
        instructions (:instructions e)]
    (conj (vec instructions) (return-instruction val))))

(defn ast-statement->tacky-instructions [statement]
  (remove nil? (ret-instructions (:value statement))))

(defn tacky-generate [ast]
  (reset! counter 0)
  (map (fn [f]
         (-> f
             (assoc :instructions (flatten (map ast-statement->tacky-instructions (:statements f))))
             (dissoc :statements)))
       ast))

(comment

  (reset! counter 0)

  (pp/pprint
   (tacky-generate
    (p/parse (l/lex "int main(void) {return 1;}"))))

  (pp/pprint
   (tacky-generate
    (p/parse "int main(void) {return 1 * 2 & 3 * (4 + 5);}")))

  (pp/pprint
   (p/parse "int main(void) {return 1 * 2 - 3 * (4 + 5);}"))

  (pp/pprint
   (p/parse "int main(void) {return 1 + 2 + -3 + -(4 + 5);}"))

  (pp/pprint
   (tacky-generate
    (p/parse "int main(void) {return 1 + 2 + -3 + -(4 + 5);}")))

  (pp/pprint
   (exp-instructions [:exp [:constant "2"]]))

  (pp/pprint
   (exp-instructions [:exp [:constant "2"]]))

  (pp/pprint
   (exp-instructions [:exp [:unop-exp [:unop "-"] [:exp [:constant "2"]]]]))

  (def ex-exp
    [:exp
     [:unop-exp
      [:unop "-"]
      [:exp
       [:unop-exp
        [:unop "~"]
        [:exp [:unop-exp [:unop "-"] [:exp [:constant "8"]]]]]]]])

  (def ex-ret
    [:statement "return"
     [:exp
      [:unop-exp
       [:unop "-"]
       [:exp
        [:unop-exp
         [:unop "~"]
         [:exp [:unop-exp [:unop "-"] [:exp [:constant "8"]]]]]]]]])

  (pp/pprint
   (exp-instructions ex-exp))

  (def exprg
    "int main(void) {
     return -(~(-8));
   }")

  (pp/pprint
   (ret-instructions ex-ret))

  ())
