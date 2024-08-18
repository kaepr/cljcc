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

(defn- unary-operator [op]
  (condp = op
    :complement :complement
    :hyphen :negate))

(defn- binary-operator [binop]
  (condp = binop
    :plus :add
    :hyphen :sub
    :multiply :mul
    :divide :div
    :remainder :mod
    :ampersand :bit-and
    :bitwise-or :bit-or
    :bitwise-xor :bit-xor
    :bitwise-right-shift :bit-right-shift
    :bitwise-left-shift :bit-left-shift))

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

(declare expression-handler)

(defn- constant-expr-handler [e]
  {:val (constant-instruction (:value e))})

(defn- unary-expr-handler [e]
  (let [inner (expression-handler (:value e))
        dst (variable)
        src (:val inner)
        unary-operator (unary-operator (:unary-operator e))
        instruction (unary-instruction unary-operator src dst)]
    {:val dst
     :instructions (flatten [(:instructions inner) instruction])}))

(defn- binary-expr-handler [e]
  (let [e1 (expression-handler (:left e))
        e2 (expression-handler (:right e))
        src1 (:val e1)
        src2 (:val e2)
        dst (variable)
        binary-operator (binary-operator (:binary-operator e))
        instruction (binary-instruction binary-operator src1 src2 dst)]
    {:val dst
     :instructions (flatten [(:instructions e1) (:instructions e2) instruction])}))

(defn- expression-handler [e]
  (when-let [exp-type (:exp-type e)]
    (cond
      (= exp-type :constant-exp) (constant-expr-handler e)
      (= exp-type :unary-exp) (unary-expr-handler e)
      (= exp-type :binary-exp) (binary-expr-handler e)
      :else (throw (ex-info "Tacky error. Invalid expression." {e e})))))

(defn- exp-instructions [exp]
  (expression-handler exp))

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
    (p/parse (l/lex "int main(void) {return -(~1);}"))))

  (pp/pprint
   (tacky-generate
    (p/parse (l/lex "int main(void) {return 1 * -2 / ~3 * (4 - 5);}"))))

  ())
