(ns cljcc.tacky
  (:require
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

(defn parsed-var->tacky-var [v]
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

(defn- fun-call-instruction [identifier arguments dst]
  {:type :fun-call
   :identifier identifier
   :arguments arguments
   :dst dst})

;;;; Expression handlers

(declare expression-handler)

(defn- constant-expr-handler [e]
  {:val (constant (:value e))})

(defn- unary-expr-handler [e]
  (let [inner (expression-handler (:value e))
        src (:val inner)
        op (unary-operator (:unary-operator e))
        dst (variable (str "unary_result_" op))
        instruction (unary-instruction op src dst)]
    {:val dst
     :instructions (flatten [(:instructions inner) instruction])}))

(defn- binary-expr-handler [e]
  (let [e1 (expression-handler (:left e))
        e2 (expression-handler (:right e))
        src1 (:val e1)
        src2 (:val e2)
        op (binary-operator (:binary-operator e))
        dst (variable (str "binary_result_" op))
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

(defn- function-call-exp-handler [{identifier :identifier arguments :arguments}]
  (let [arg-exps (mapv expression-handler arguments)
        dst (variable (str "function_call_result_" identifier))
        fn-instruction (fun-call-instruction identifier (mapv #(:val %) arg-exps) dst)]
    {:val dst
     :instructions (flatten [(mapv #(:instructions %) arg-exps) fn-instruction])}))

(defn- expression-handler [e]
  (when-let [exp-type (:exp-type e)]
    (condp = exp-type
      :constant-exp (constant-expr-handler e)
      :unary-exp (unary-expr-handler e)
      :binary-exp (let [op (:binary-operator e)]
                    (condp = op
                      :logical-and (logical-and-handler e)
                      :logical-or (logical-or-handler e)
                      (binary-expr-handler e)))
      :variable-exp {:val (parsed-var->tacky-var e)}
      :assignment-exp (assignment-exp-handler e)
      :conditional-exp (conditional-exp-handler e)
      :function-call-exp (function-call-exp-handler e)
      (throw (ex-info "Tacky error. Invalid expression." {e e})))))

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
  (flatten (mapv block-item->tacky-instruction (:block s))))

(defn- break-statement-handler [s]
  [(jump-instruction (str "break_" (:label s)))])

(defn- continue-statement-handler [s]
  [(jump-instruction (str "continue_" (:label s)))])

(defn- while-statement-handler [s]
  (let [continue-label (str "continue_" (:label s))
        break-label (str "break_" (:label s))
        cond-exp (exp-instructions (:condition s))
        cond-value (:val cond-exp)
        cond-instructions (:instructions cond-exp)
        body-instructions (statement->tacky-instruction (:body s))]
    (flatten [(label-instruction continue-label)
              cond-instructions
              (jump-if-zero-instruction cond-value break-label)
              body-instructions
              (jump-instruction continue-label)
              (label-instruction break-label)])))

(defn- do-while-statement-handler [s]
  (let [start-label (label "do_while_start")
        continue-label (str "continue_" (:label s))
        break-label (str "break_" (:label s))
        cond-exp (exp-instructions (:condition s))
        cond-value (:val cond-exp)
        cond-instructions (:instructions cond-exp)
        body-instructions (statement->tacky-instruction (:body s))]
    (flatten [(label-instruction start-label)
              body-instructions
              (label-instruction continue-label)
              cond-instructions
              (jump-if-not-zero-instruction cond-value start-label)
              (label-instruction break-label)])))

(defn- for-statement-handler [s]
  (let [init-instructions (if (= :declaration (:type (:init s)))
                            (block-item->tacky-instruction (:init s))
                            (:instructions (exp-instructions (:init s))))
        start-label (label "for_start")
        break-label (str "break_" (:label s))
        continue-label (str "continue_" (:label s))
        cond? (not (nil? (:condition s)))
        body-instructions (statement->tacky-instruction (:body s))
        post-instructions (if (nil? (:post s))
                            []
                            (:instructions (exp-instructions (:post s))))
        cond-instructions (if cond?
                            (let [ce (exp-instructions (:condition s))
                                  ce-inst (:instructions ce)
                                  ce-v (:val ce)]
                              [ce-inst
                               (jump-if-zero-instruction ce-v break-label)])
                            [])]
    (flatten
     [init-instructions
      (label-instruction start-label)
      cond-instructions
      body-instructions
      (label-instruction continue-label)
      post-instructions
      (jump-instruction start-label)
      (label-instruction break-label)])))

(defn- statement->tacky-instruction [s]
  (condp = (:statement-type s)
    :return (let [e (exp-instructions (:value s))
                  val (:val e)
                  instructions (:instructions e)]
              (conj (vec instructions) (return-instruction val)))
    :expression [(:instructions (exp-instructions (:value s)))]
    :if (if-statement-handler s)
    :compound (compound-statement-handler s)
    :break (break-statement-handler s)
    :continue (continue-statement-handler s)
    :for (for-statement-handler s)
    :while (while-statement-handler s)
    :do-while (do-while-statement-handler s)
    :empty []
    (throw (ex-info "Tacky error. Invalid statement." {:statement s}))))

(defn- declaration->tacky-instruction [d]
  (when (:initial d)
    (let [local? (nil? (:storage-class d))
          var (parsed-var->tacky-var d) ; only needs :identifier key in declaration
          rhs (exp-instructions (:initial d))]
      (if local?
        (flatten [(:instructions rhs) (copy-instruction (:val rhs) var)])
        [])))) ; ignoring initializers for non local variable declarations

(defn- block-item->tacky-instruction [item]
  (condp = (:type item)
    :statement (statement->tacky-instruction item)
    :declaration (declaration->tacky-instruction item)
    (throw (ex-info "Tacky error. Invalid block item." {:item item}))))

(defn- function-definition->tacky-function [function-definition ident->symbol]
  (let [add-return (fn [xs] (conj (vec xs) (return-instruction (constant 0))))
        instructions (->> function-definition
                          :body
                          (remove nil?)
                          (mapv block-item->tacky-instruction)
                          flatten
                          (remove nil?)
                          add-return)]
    (-> function-definition
        (dissoc :body)
        (assoc :global? (get-in ident->symbol [(:identifier function-definition)
                                               :attrs
                                               :global?]))
        (assoc :instructions instructions))))

(defn- tacky-static-variable [identifier global? initial-value]
  {:identifier identifier
   :global? global?
   :initial-value initial-value
   :type :declaration
   :declaration-type :static-variable})

(defn- tacky-static-variable-instructions [ident->symbols]
  (reduce
   (fn [acc [k v]]
     (if (string? k)
       (if (= :static (get-in v [:attrs :type]))
         (condp = (get-in v [:attrs :initial-value :type])
           :initial (conj acc (tacky-static-variable k (get-in v [:attrs :global?]) (get-in v [:attrs :initial-value :value])))
           :tentative (conj acc (tacky-static-variable k (get-in v [:attrs :global?]) 0))
           acc)
         acc)
       acc))
   []
   ident->symbols))

(defn- tacky-function-instructions [ast ident->symbol]
  (let [fn-defined? (fn [x] (if (= :function (:declaration-type x))
                              (or (= (:identifier x) "main") (seq (:body x)))
                              true))]
    (->> ast
         (filterv #(= :function (:declaration-type %)))
         (filterv fn-defined?)
         (mapv #(function-definition->tacky-function % ident->symbol)))))

(defn tacky-generate [{ast :block ident->symbol :ident->symbol}]
  (let [variable-instructions (tacky-static-variable-instructions ident->symbol)
        function-instructions (tacky-function-instructions ast ident->symbol)]
    {:program (concat variable-instructions function-instructions)
     :ident->symbol ident->symbol}))

(defn tacky-from-src [src]
  (-> src
      l/lex
      p/parse
      a/validate
      tacky-generate))

(comment

  (tacky-from-src
   "
extern int foo;

int foo;

int foo;

int main(void) {
    for (int i = 0; i < 5; i = i + 1)
        foo = foo + 1;
    return foo;
}

int foo;

")

  ())
