(ns cljcc.tacky
  (:require
   [cljcc.lexer :as l]
   [cljcc.util :as u]
   [cljcc.parser :as p]
   [cljcc.exception :as exc]
   [cljcc.symbol :as sym]
   [cljcc.analyze.resolve :as r]
   [cljcc.analyze.label-loops :as label-loop]
   [malli.dev.pretty :as pretty]
   [cljcc.analyze.typecheck :as tc]
   [cljcc.analyze.core :as a]
   [malli.core :as m]
   [cljcc.schema :as s]))

(defn- variable
  ([]
   (variable "var"))
  ([identifier]
   {:type :variable
    :value (u/create-identifier! (str identifier))}))

(defn parsed-var->tacky-var [v]
  {:type :variable
   :value (:identifier v)})

(defn tacky-var [identifier]
  {:type :variable
   :value identifier})

(defn- label
  ([] (label "label"))
  ([ident] (u/create-identifier! ident)))

(defn- const-int [v]
  {:type :int
   :value v})

(defn- const-long [v]
  {:type :long
   :value v})

(defn constant [const-value]
  {:type :constant
   :value const-value})

(defn- unary-operator
  "Converts parser's unary operator to tacky representation."
  [op]
  (condp = op
    :complement :bit-not
    :hyphen :negate
    :logical-not :logical-not
    (exc/tacky-error "Invalid unary operator." {op op})))

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
    :assignment-bitwise-right-shift :bitwise-right-shift
    (exc/tacky-error "Invalid assignment operator." op)))

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
    (exc/tacky-error "Invalid binary operator." binop)))

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

(defn- sign-extend-instruction [src dst]
  {:type :sign-extend
   :src src
   :dst dst})

(defn- truncate-instruction [src dst]
  {:type :truncate
   :src src
   :dst dst})

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

;; Timothy Baldridge, Data all the ASTs
(defn postwalk [ast f]
  (f (reduce
      (fn [acc key]
        (let [value (get acc key)]
          (if (vector? value)
            (assoc acc key (doall (map (fn [node] (postwalk node f))
                                       value)))
            (assoc acc key (postwalk value f)))))
      ast
      (:children ast))))

(defn- add-var-to-symbol [var var-type symbols]
  (swap! symbols assoc (:value var) {:type var-type
                                     :attribute (sym/local-attribute)}))

(defmulti exp-handler
  (fn [exp _symbols]
    (:exp-type exp)))

(comment

  (exp-handler
   {:type :exp,
    :exp-type :function-call-exp,
    :children [:arguments],
    :identifier "foo",
    :arguments [],
    :value-type {:type :long}}
   (atom {}))

  ())

(comment

  (exp-handler
   {:type :exp,
    :exp-type :variable-exp,
    :identifier "x.5",
    :value-type {:type :int}}
   (atom {}))

  ())

(defmethod exp-handler :default
  [_ _]
  {:instructions []})

(defmethod exp-handler :constant-exp
  [exp _]
  {:val (constant (:value exp))})

(defmethod exp-handler :variable-exp
  [exp _]
  {:val (tacky-var (:identifier exp))})

(defmethod exp-handler :cast-exp
  [{:keys [target-type value typed-inner]} symbols]
  (if (= target-type (tc/get-type typed-inner))
    value
    (let [dst (variable "cast_")
          _ (add-var-to-symbol dst target-type symbols)
          {res :val
           insts :instructions} value]
      (if (= :long (:type target-type))
        {:val dst
         :instructions (flatten [insts
                                 (sign-extend-instruction res dst)])}
        {:val dst
         :instructions (flatten [insts
                                 (truncate-instruction res dst)])}))))

(defmethod exp-handler :unary-exp
  [exp symbols]
  (let [{src :val
         insts :instructions} (:value exp)
        op (unary-operator (:unary-operator exp))
        dst (variable (str "unary_result_" op))
        _ (add-var-to-symbol dst (tc/get-type exp) symbols)
        inst (unary-instruction op src dst)]
    {:val dst
     :instructions (flatten [insts inst])}))

(defn logical-and-binary-handler
  [exp symbols]
  (let [{v1 :val
         insts1 :instructions} (:left exp)
        {v2 :val
         insts2 :instructions} (:right exp)
        res (variable "and_result")
        _ (add-var-to-symbol res (tc/get-type exp) symbols)
        false-label (label "and_false")
        end-label (label "and_end")]
    {:val res
     :instructions (flatten [insts1
                             (jump-if-zero-instruction v1 false-label)
                             insts2
                             (jump-if-zero-instruction v2 false-label)
                             (copy-instruction (constant (const-int 1)) res)
                             (jump-instruction end-label)
                             (label-instruction false-label)
                             (copy-instruction (constant (const-int 0)) res)
                             (label-instruction end-label)])}))

(defn logical-or-binary-handler
  [exp symbols]
  (let [{v1 :val
         insts1 :instructions} (:left exp)
        {v2 :val
         insts2 :instructions} (:right exp)
        res (variable "or_result")
        _ (add-var-to-symbol res (tc/get-type exp) symbols)
        false-label (label "or_false")
        end-label (label "or_end")]
    {:val res
     :instructions (flatten [insts1
                             (jump-if-not-zero-instruction v1 end-label)
                             insts2
                             (jump-if-not-zero-instruction v2 end-label)
                             (copy-instruction (constant (const-int 0)) res)
                             (jump-instruction false-label)
                             (label-instruction end-label)
                             (copy-instruction (constant (const-int 1)) res)
                             (label-instruction false-label)])}))

(defn binary-exp-handler
  [exp symbols]
  (let [{v1 :val
         insts1 :instructions} (:left exp)
        {v2 :val
         insts2 :instructions} (:right exp)
        op (binary-operator (:binary-operator exp))
        dst (variable (str "binary_result_" op))
        _ (add-var-to-symbol dst (tc/get-type exp) symbols)
        binary-inst (binary-instruction op v1 v2 dst)]
    {:val dst
     :instructions (flatten [insts1
                             insts2
                             binary-inst])}))

(defmethod exp-handler :binary-exp
  [exp symbols]
  (let [op (:binary-operator exp)]
    (condp = op
      :logical-and (logical-and-binary-handler exp symbols)
      :logical-or (logical-or-binary-handler exp symbols)
      (binary-exp-handler exp symbols))))

(defmethod exp-handler :assignment-exp
  [exp symbols]
  (let [op (:assignment-operator exp)
        var (:val (:left exp)); guaranteed to be a TackyVariable
        direct-assignment? (= op :assignment)]
    (if direct-assignment?
      (let [{dst :val
             insts :instructions} (:right exp)]
        {:val var
         :instructions (flatten [insts
                                 (copy-instruction dst var)])})
      (let [bin-op (assignment-operator->binary-operator op)
            bin-exp (p/binary-exp-node (:left exp) (:right exp) bin-op)
            {rhs :val
             insts :instructions} (exp-handler bin-exp symbols)]
        {:val rhs
         :instructions (flatten [insts
                                 (copy-instruction rhs var)])}))))

(defmethod exp-handler :conditional-exp
  [exp symbols]
  (let [{condition-val :val
         condition-insts :instructions} (:left exp)
        {then-val :val
         then-insts :instructions} (:middle exp)
        {else-val :val
         else-insts :instructions} (:right exp)
        end-label (label "condition_end")
        else-label (label "conditional_else")
        res (variable "conditional_result")
        _ (add-var-to-symbol res (tc/get-type exp) symbols)]
    {:val res
     :instructions (flatten [condition-insts
                             (jump-if-zero-instruction condition-val else-label)
                             then-insts
                             (copy-instruction then-val res)
                             (jump-instruction end-label)
                             (label-instruction else-label)
                             else-insts
                             (copy-instruction else-val res)
                             (label-instruction end-label)])}))

(defmethod exp-handler :function-call-exp
  [{identifier :identifier
    arguments :arguments :as exp} symbols]
  (let [dst (variable (str "function_call_result_" identifier))
        _ (add-var-to-symbol dst (tc/get-type exp) symbols)
        fn-instruction (fun-call-instruction identifier
                                             (mapv #(:val %) arguments)
                                             dst)]
    {:val dst
     :instructions (flatten [(mapv #(:instructions %) arguments)
                             fn-instruction])}))

(defn run-expression-handler
  "Transforms a expression to tacky variable and instructions.

  Parameters:
    exp: Expression to be parsed
    symbols: Atom for symbol map"
  [exp symbols]
  (postwalk exp #(exp-handler % symbols)))

(comment

  ())

;;;; Statement Handlers

(declare statement->tacky-instruction block-item->tacky-instruction)

(defn if-statement-handler [s symbols]
  (let [cond-exp (run-expression-handler (:condition s) symbols)
        cond-value (:val cond-exp)
        cond-instructions (:instructions cond-exp)
        then-instructions (statement->tacky-instruction (:then-statement s) symbols)
        end-label (label "if_end")
        else-label (label "if_else")
        else? (:else-statement s)]
    (if else?
      [cond-instructions
       (jump-if-zero-instruction cond-value else-label)
       then-instructions
       (jump-instruction end-label)
       (label-instruction else-label)
       (statement->tacky-instruction (:else-statement s) symbols)
       (label-instruction end-label)]
      [cond-instructions
       (jump-if-zero-instruction cond-value end-label)
       then-instructions
       (label-instruction end-label)])))

(defn- compound-statement-handler [s symbols]
  (flatten (mapv #(block-item->tacky-instruction % symbols) (:block s))))

(defn- break-statement-handler [s _]
  [(jump-instruction (str "break_" (:label s)))])

(defn- continue-statement-handler [s _]
  [(jump-instruction (str "continue_" (:label s)))])

(defn- while-statement-handler [s symbols]
  (let [continue-label (str "continue_" (:label s))
        break-label (str "break_" (:label s))
        cond-exp (run-expression-handler (:condition s) symbols)
        cond-value (:val cond-exp)
        cond-instructions (:instructions cond-exp)
        body-instructions (statement->tacky-instruction (:body s) symbols)]
    (flatten [(label-instruction continue-label)
              cond-instructions
              (jump-if-zero-instruction cond-value break-label)
              body-instructions
              (jump-instruction continue-label)
              (label-instruction break-label)])))

(defn- do-while-statement-handler [s symbols]
  (let [start-label (label "do_while_start")
        continue-label (str "continue_" (:label s))
        break-label (str "break_" (:label s))
        cond-exp (run-expression-handler (:condition s) symbols)
        cond-value (:val cond-exp)
        cond-instructions (:instructions cond-exp)
        body-instructions (statement->tacky-instruction (:body s) symbols)]
    (flatten [(label-instruction start-label)
              body-instructions
              (label-instruction continue-label)
              cond-instructions
              (jump-if-not-zero-instruction cond-value start-label)
              (label-instruction break-label)])))

(defn- for-statement-handler [s symbols]
  (let [init-instructions (if (= :declaration (:type (:init s)))
                            (block-item->tacky-instruction (:init s) symbols)
                            (:instructions (run-expression-handler (:init s) symbols)))
        start-label (label "for_start")
        break-label (str "break_" (:label s))
        continue-label (str "continue_" (:label s))
        cond? (not (nil? (:condition s)))
        body-instructions (statement->tacky-instruction (:body s) symbols)
        post-instructions (if (nil? (:post s))
                            []
                            (:instructions (run-expression-handler (:post s) symbols)))
        cond-instructions (if cond?
                            (let [ce (run-expression-handler (:condition s) symbols)
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

(defn- statement->tacky-instruction [s symbols]
  (condp = (:statement-type s)
    :return (let [e (run-expression-handler (:value s) symbols)
                  val (:val e)
                  instructions (:instructions e)]
              (conj (vec instructions) (return-instruction val)))
    :expression [(:instructions (run-expression-handler (:value s) symbols))]
    :if (if-statement-handler s symbols)
    :compound (compound-statement-handler s symbols)
    :break (break-statement-handler s symbols)
    :continue (continue-statement-handler s symbols)
    :for (for-statement-handler s symbols)
    :while (while-statement-handler s symbols)
    :do-while (do-while-statement-handler s symbols)
    :empty []
    (exc/tacky-error "Invalid statement" s)))

(defn- declaration->tacky-instruction [d symbols]
  (when (:initial d)
    (let [local? (nil? (:storage-class d))
          var (parsed-var->tacky-var d) ; only needs :identifier key in declaration
          rhs (run-expression-handler (:initial d) symbols)]
      (if local?
        (flatten [(:instructions rhs) (copy-instruction (:val rhs) var)])
        [])))) ; ignoring initializers for non local variable declarations

(defn- block-item->tacky-instruction [item symbols]
  (condp = (:type item)
    :statement (statement->tacky-instruction item symbols)
    :declaration (declaration->tacky-instruction item symbols)
    (exc/tacky-error "Invalid block item." item)))

(defn- function-definition->tacky-function [function-definition symbols]
  (let [add-return (fn [xs] (conj (vec xs) (return-instruction (constant {:type :int :value 0}))))
        instructions (->> function-definition
                          :body
                          (remove nil?)
                          (mapv #(block-item->tacky-instruction % symbols))
                          flatten
                          (remove nil?)
                          add-return)]
    (-> function-definition
        (dissoc :body)
        (assoc :global? (get-in @symbols [(:identifier function-definition)
                                          :attribute
                                          :global?]))
        (assoc :instructions instructions))))

(defn- tacky-static-variable [identifier global? variable-type initial]
  {:identifier identifier
   :global? global?
   :initial initial
   :type :declaration
   :variable-type variable-type
   :declaration-type :static-variable})

(defn- tacky-static-variable-instructions
  "Generates list of tacky static variable from symbol map."
  [ident->symbol]
  (let [rf (fn [acc [k v]]
             (if (= :static (get-in v [:attribute :type]))
               (let [vtype (get-in v [:type])
                     global? (get-in v [:attribute :global?])
                     initial (get-in v [:attribute :initial-value])
                     tentative-initial (if (= :int (:type vtype))
                                         (sym/initial-iv (sym/int-init 0))
                                         (sym/initial-iv (sym/long-init 0)))
                     itype (get-in v [:attribute :initial-value :type])]
                 (condp = itype
                   :initial (conj acc (tacky-static-variable k global? vtype initial))
                   :tentative (conj acc (tacky-static-variable k global? vtype tentative-initial))
                   acc))
               acc))]
    (reduce rf [] ident->symbol)))

(defn- tacky-function-instructions [ast symbols]
  (let [fn-defined? (fn [x] (if (= :function (:declaration-type x))
                              (or (= (:identifier x) "main") (seq (:body x)))
                              true))]
    (->> ast
         (filterv #(= :function (:declaration-type %)))
         (filterv fn-defined?)
         (mapv #(function-definition->tacky-function % symbols)))))

(defn tacky-generate [{ast :program ident->symbol :ident->symbol}]
  (let [variable-instructions (tacky-static-variable-instructions ident->symbol)
        symbols (atom ident->symbol)
        function-instructions (tacky-function-instructions ast symbols)
        program (vec (concat variable-instructions function-instructions))
        _ (m/coerce s/TackyProgram program)
        _ (m/coerce s/SymbolMap @symbols)]
    {:program program
     :ident->symbol @symbols}))

(defn tacky-from-src [src]
  (-> src
      l/lex
      p/parse
      a/validate
      tacky-generate))

(comment

  (def tmp
    "
long foo(void) {
    return 1;
}

int bar(int x, int y) {
 return x + y;
}

int main(void) {
int x = 6;
return (long) foo() + 2 + x + bar(x, 7) + (3 ? 4 : 5);

}")

  (-> tmp
      l/lex
      p/parse
      a/validate)

  (tacky-from-src tmp)

  ())

(comment

  (def ex "
long foo(void) {
    return 1;
}

int bar(int x, int y) {
 return x + y;
}

int main(void) {
int x = 6;
return (long) foo() + 2 + x + bar(x, 7) + (3 ? 4 : 5);
}")

  (-> ex
      l/lex
      p/parse
      a/validate)

  (tacky-from-src
   "int main(void) { return 42; }")

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

  (def file-path "./test-programs/example.c")

  (slurp "./test-programs/example.c")

  (-> file-path
      slurp
      p/parse-from-src
      a/validate)

  (-> file-path
      slurp
      p/parse-from-src
      a/validate
      tacky-generate)

  (pretty/explain
   s/TackyProgram
   (-> file-path
       slurp
       p/parse-from-src
       a/validate
       tacky-generate
       :program))

  (-> file-path
      slurp
      p/parse-from-src
      a/validate
      tacky-generate)

  ())
