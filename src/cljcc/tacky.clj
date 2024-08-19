(ns cljcc.tacky
  (:require
   [clojure.pprint :as pp]
   [cljcc.lexer :as l]
   [clojure.string :as s]
   [cljcc.parser :as p]))

(def counter "Global integer counter for generating unique identifier names." (atom 0))

(defn- create-identifier
  "Returns a unique identifier. Used for generating tacky variable names.

  Removes : from keywords.
  Replaces all - with _ for generating valid assembly names."
  ([]
   (create-identifier "tmp"))
  ([identifier]
   (let [n @counter
         _ (swap! counter #(+ % 1))]
     (-> identifier
         (str "." n)
         (s/replace #":" "")
         (s/replace #"-" "_")))))

(defn- variable
  ([]
   {:type :variable
    :value (create-identifier "var")})
  ([identifier]
   {:type :variable
    :value (create-identifier (str identifier))}))

(defn- label
  ([] (create-identifier "label"))
  ([ident] (create-identifier ident)))

(defn constant [^Integer v]
  {:type :constant
   :value v})

(defn- unary-operator
  "Converts parser's unary operator to tacky representation."
  [op]
  (condp = op
    :complement :bit-not
    :hyphen :negate
    :logical-not :logical-not
    (throw (ex-info "Tacky Error. Invalid unary operator." {op op}))))

(defn- binary-operator
  "Converts parser's binary operator to tacky representation."
  [binop]
  (condp = binop
    :plus :add
    :hyphen :sub
    :multiply :mul
    :divide :div
    :remainder :mod
    :equal-to :equal
    :not-equal-to :not-equal
    :less-than :less-than
    :greater-than :greater-than
    :less-than-equal-to :less-or-equal
    :greater-than-equal-to :greater-or-equal
    :ampersand :bit-and
    :bitwise-or :bit-or
    :bitwise-xor :bit-xor
    :bitwise-right-shift :bit-right-shift
    :bitwise-left-shift :bit-left-shift
    (throw (ex-info "Tacky Error. Invalid binary operator." {binop binop}))))

;;;; Instructions

(defn- unary-instruction [op src dst]
  {:type :unary
   :unary-operator op
   :dst dst
   :src src})

(defn- binary-instruction [op src1 src2 dst]
  {:type :binary
   :binary-operator op
   :src1 src1
   :src2 src2
   :dst dst})

(defn- return-instruction [val]
  {:type :return
   :val val})

(defn- copy-instruction [src dst]
  {:type :copy
   :src src
   :dst dst})

(defn- jump-instruction [target]
  {:type :jump
   :identifier target})

(defn- jump-if-zero-instruction [condition target]
  {:type :jump-if-zero
   :identifier target
   :val condition})

(defn- jump-if-not-zero-instruction [condition target]
  {:type :jump-if-not-zero
   :identifier target
   :val condition})

(defn- label-instruction [identifier]
  {:type :label
   :identifier identifier})

;;;; Expression handlers

(declare expression-handler)

(defn- constant-expr-handler [e]
  {:val (constant (:value e))})

(defn- unary-expr-handler [e]
  (let [inner (expression-handler (:value e))
        src (:val inner)
        op (unary-operator (:unary-operator e))
        dst (variable (str "var_" op))
        instruction (unary-instruction op src dst)]
    {:val dst
     :instructions (flatten [(:instructions inner) instruction])}))

(defn- binary-expr-handler [e]
  (let [e1 (expression-handler (:left e))
        e2 (expression-handler (:right e))
        src1 (:val e1)
        src2 (:val e2)
        op (binary-operator (:binary-operator e))
        dst (variable (str "var_" op))
        instruction (binary-instruction op src1 src2 dst)]
    {:val dst
     :instructions (flatten [(:instructions e1) (:instructions e2) instruction])}))

(defn- logical-and-handler [e]
  (let [e1 (expression-handler (:left e))
        e2 (expression-handler (:right e))
        v1 (:val e1)
        v2 (:val e2)
        res (variable "and_result")
        false-label (label "and_false")
        end-label (label "and_end")]
    {:val res
     :instructions (flatten [(:instructions e1)
                             (jump-if-zero-instruction v1 false-label)
                             (:instructions e2)
                             (jump-if-zero-instruction v2 false-label)
                             (copy-instruction (constant 1) res)
                             (jump-instruction end-label)
                             (label-instruction false-label)
                             (copy-instruction (constant 0) res)
                             (label-instruction end-label)])}))

(defn- logical-or-handler [e]
  (let [e1 (expression-handler (:left e))
        e2 (expression-handler (:right e))
        v1 (:val e1)
        v2 (:val e2)
        res (variable "or_result")
        false-label (label "or_false")
        end-label (label "or_end")]
    {:val res
     :instructions (flatten [(:instructions e1)
                             (jump-if-not-zero-instruction v1 end-label)
                             (:instructions e2)
                             (jump-if-not-zero-instruction v2 end-label)
                             (copy-instruction (constant 0) res)
                             (jump-instruction false-label)
                             (label-instruction end-label)
                             (copy-instruction (constant 1) res)
                             (label-instruction false-label)])}))

(defn- expression-handler [e]
  (when-let [exp-type (:exp-type e)]
    (cond
      (= exp-type :constant-exp) (constant-expr-handler e)
      (= exp-type :unary-exp) (unary-expr-handler e)
      (= exp-type :binary-exp) (let [op (:binary-operator e)]
                                 (condp = op
                                   :logical-and (logical-and-handler e)
                                   :logical-or (logical-or-handler e)
                                   (binary-expr-handler e)))
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

  (pp/pprint
   (tacky-generate
    (p/parse (l/lex "int main(void) {return (1 + 2) || (3 + 4);}"))))

  ())
