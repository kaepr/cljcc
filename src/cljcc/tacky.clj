(ns cljcc.tacky
  (:require
   [clojure.pprint :as pp]
   [cljcc.lexer :as l]
   [cljcc.util :as u]
   [cljcc.parser :as p]
   [cljcc.analyzer :as a]))

(defn- variable
  ([]
   (variable "var"))
  ([identifier]
   {:type :variable
    :value (u/create-identifier! (str identifier))}))

(defn- parsed-var->tacky-var [v]
  {:type :variable
   :value (:identifier v)})

(defn- label
  ([] (label "label"))
  ([ident] (u/create-identifier! ident)))

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

(defn- assignment-operator
  "Converts parser assignment operator to tacky representation."
  [op]
  (condp = op
    :assignemnt :assignemnt
    :assignment-plus :assignment-add
    :assignment-multiply :assignment-mul
    :assignment-minus :assignment-minus
    :assignment-divide :assignment-div
    :assignment-mod :assignment-mod
    :assignment-bitwise-and :assignemnt-bit-and
    :assignment-bitwise-or :assignemnt-bit-or
    :assignment-bitwise-xor :assignemnt-bit-xor
    :assignment-bitwise-left-shift :assignment-bit-left-shift
    :assignment-bitwise-right-shift :assignment-bit-right-shift))

(defn- assignment-operator->binary-operator
  "Converts parser assignment operator to binary operator keyword."
  [op]
  (condp = op
    :assignemnt :assignemnt
    :assignment-plus :plus
    :assignment-multiply :multiply
    :assignment-minus :hyphen
    :assignment-divide :divide
    :assignment-mod :remainder
    :assignment-bitwise-and :ampersand
    :assignment-bitwise-or :bitwise-or
    :assignment-bitwise-xor :bitwise-xor
    :assignment-bitwise-left-shift :bitwise-left-shift
    :assignment-bitwise-right-shift :bitwise-right-shift))

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

(defn- assignment-exp-handler [e]
  (let [asop (:assignment-operator e)
        direct-assignment? (= asop :assignment)
        var (parsed-var->tacky-var (:left e))] ; guaranteed to be parsed variable
    (if direct-assignment?
      (let [rhs (expression-handler (:right e))]
        {:val var
         :instructions (flatten [(:instructions rhs)
                                 (copy-instruction (:val rhs) var)])})
      (let [bin-op (assignment-operator->binary-operator (:assignment-operator e))
            bin-exp (p/binary-exp-node (:left e) (:right e) bin-op)
            rhs (expression-handler bin-exp)]
        {:val var
         :instructions (flatten [(:instructions rhs)
                                 (copy-instruction (:val rhs) var)])}))))

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
      (= exp-type :variable-exp) {:val (parsed-var->tacky-var e)}
      (= exp-type :assignment-exp) (assignment-exp-handler e)
      :else (throw (ex-info "Tacky error. Invalid expression." {e e})))))

(defn- exp-instructions [exp]
  (expression-handler exp))

(defn- statement->tacky-instruction [s]
  (condp = (:statement-type s)
    :return (let [e (exp-instructions (:value s))
                  val (:val e)
                  instructions (:instructions e)]
              (conj (vec instructions) (return-instruction val)))
    :expression [(:instructions (exp-instructions (:value s)))]
    :empty []
    (throw (ex-info "Tacky error. Invalid statement." {:statement s}))))

(defn- declaration->tacky-instruction [d]
  (when (:initial d)
    (let [var (parsed-var->tacky-var d) ; only needs :identifier key in declaration
          rhs (exp-instructions (:initial d))]
      (flatten [(:instructions rhs) (copy-instruction (:val rhs) var)]))))

(defn- block-item->tacky-instruction [item]
  (condp = (:type item)
    :statement (statement->tacky-instruction item)
    :declaration (declaration->tacky-instruction item)
    (throw (ex-info "Tacky error. Invalid block item." {:item item}))))

(defn- function-body->tacky-instructions [body]
  (let [v (->> body
               (remove nil?)
               (map block-item->tacky-instruction)
               flatten
               (remove nil?))]
    (conj (vec v) (return-instruction (constant 0)))))

(defn tacky-generate [ast]
  (map (fn [f]
         (-> f
             (assoc :instructions (function-body->tacky-instructions (:body f)))
             (dissoc :body)))
       ast))

(defn tacky-from-src [src]
  (-> src
      l/lex
      p/parse
      a/validate
      tacky-generate))

(comment

  (pp/pprint
   (tacky-from-src
    "int main(void) {
int a = 11;
    int b = 12;
    a &= 0 || b;
    b ^= a || 1;

    int c = 14;
    c |= a || b;

    int d = 16;
    d >>= c || d;

    int e = 18;
    e <<= c || d;
    return (a == 1 && b == 13 && c == 15 && d == 8 && e == 36);
}"))

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
