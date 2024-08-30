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

(defn- conditional-exp-handler [e]
  (let [ce (expression-handler (:left e))
        cv (:val ce)
        then-e (expression-handler (:middle e))
        else-e (expression-handler (:right e))
        end-label (label "conditional_end")
        else-label (label "conditional_else")
        res (variable "conditional_result")]
    {:val res
     :instructions (flatten
                    [(:instructions ce)
                     (jump-if-zero-instruction cv else-label)
                     (:instructions then-e)
                     (copy-instruction (:val then-e) res)
                     (jump-instruction end-label)
                     (label-instruction else-label)
                     (:instructions else-e)
                     (copy-instruction (:val else-e) res)
                     (label-instruction end-label)])}))

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
      (= exp-type :conditional-exp) (conditional-exp-handler e)
      :else (throw (ex-info "Tacky error. Invalid expression." {e e})))))

(defn- exp-instructions [exp]
  (expression-handler exp))

(declare statement->tacky-instruction block-item->tacky-instruction)

(defn if-statement-handler [s]
  (let [cond-exp (exp-instructions (:condition s))
        cond-value (:val cond-exp)
        cond-instructions (:instructions cond-exp)
        then-instructions (statement->tacky-instruction (:then-statement s))
        end-label (label "if_end")
        else-label (label "if_else")
        else? (:else-statement s)]
    (if else?
      [cond-instructions
       (jump-if-zero-instruction cond-value else-label)
       then-instructions
       (jump-instruction end-label)
       (label-instruction else-label)
       (statement->tacky-instruction (:else-statement s))
       (label-instruction end-label)]
      [cond-instructions
       (jump-if-zero-instruction cond-value end-label)
       then-instructions
       (label-instruction end-label)])))

(defn- compound-statement-handler [s]
  (flatten (map block-item->tacky-instruction (:block s))))

(defn- statement->tacky-instruction [s]
  (condp = (:statement-type s)
    :return (let [e (exp-instructions (:value s))
                  val (:val e)
                  instructions (:instructions e)]
              (conj (vec instructions) (return-instruction val)))
    :expression [(:instructions (exp-instructions (:value s)))]
    :if (if-statement-handler s)
    :compound (compound-statement-handler s)
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

(def ex
  {:type :function,
   :return-type "int",
   :identifier "main",
   :parameters :kw-void,
   :body
   [{:type :declaration,
     :identifier "a.0",
     :initial {:type :exp, :exp-type :constant-exp, :value 3}}
    {:type :statement,
     :statement-type :compound,
     :block
     [{:type :declaration,
       :identifier "a.1",
       :initial
       {:type :exp,
        :exp-type :assignment-exp,
        :assignment-operator :assignment,
        :left {:type :exp, :exp-type :variable-exp, :identifier "a.1"},
        :right {:type :exp, :exp-type :constant-exp, :value 4}}}]}
    {:type :statement,
     :statement-type :return,
     :value {:type :exp, :exp-type :variable-exp, :identifier "a.0"}}]})

(comment

  (pp/pprint
   (tacky-from-src
    "int main (void) {
int a = 3;
{
  int a = a = 4;
}
return a;
}"))

  (pp/pprint
   (tacky-generate
    (p/parse (l/lex "int main(void) {
int a = 1;
return 1;}"))))

  (pp/pprint
   (tacky-generate
    (p/parse (l/lex "int main(void) {return 1 * -2 / ~3 * (4 - 5);}"))))

  (pp/pprint
   (tacky-generate
    (p/parse (l/lex "int main(void) {return (1 + 2) || (3 + 4);}"))))

  ())
