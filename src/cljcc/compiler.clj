(ns cljcc.compiler
  (:require [cljcc.parser :as p]
            [cljcc.tacky :as t]
            [cljcc.lexer :as l]
            [cljcc.analyzer :as a]
            [cljcc.exception :as exc]))

(def registers #{:ax :dx :di :si :r8 :r9 :r10 :r11 :cx :cl})

(def cond-codes #{:e :ne :g :ge :l :le})

;;;; Instructions

(defn- mov-instruction [src dst]
  {:op  :mov
   :src src
   :dst dst})

(defn- unary-instruction [unary-operator operand]
  {:op :unary
   :unary-operator unary-operator
   :operand operand})

(defn- binary-instruction [binop src dst]
  {:op :binary
   :binary-operator binop
   :src src
   :dst dst})

(defn- cmp-instruction [src dst]
  {:op :cmp
   :src src
   :dst dst})

(defn- cdq-instruction []
  {:op :cdq})

(defn- idiv-instruction [operand]
  {:op :idiv
   :operand operand})

(defn- jmp-instruction [identifier]
  {:op :jmp
   :identifier identifier})

(defn- jmpcc-instruction [cond-code identifier]
  {:pre [(contains? cond-codes cond-code)]}
  {:op :jmpcc
   :identifier identifier
   :cond-code cond-code})

(defn- setcc-instruction [cond-code operand]
  {:pre [(contains? cond-codes cond-code)]}
  {:op :setcc
   :operand operand
   :cond-code cond-code})

(defn- label-instruction [identifier]
  {:op :label
   :identifier identifier})

(defn- allocate-stack-instruction [v]
  {:op :allocate-stack
   :value v})

(defn- deallocate-stack-instruction [v]
  {:op :deallocate-stack
   :value v})

(defn- push-instruction [operand]
  {:op :push
   :operand operand})

(defn- call-instruction [identifier]
  {:op :call
   :identifier identifier})

(defn- ret-instruction []
  {:op :ret})

;;;; Operands

;; TODO: Cleanup :operand key

(defn- imm-operand [v]
  {:operand :imm
   :operand-type :imm
   :value v})

(defn- reg-operand [reg]
  {:pre [(contains? registers reg)]}
  {:operand :reg
   :operand-type :reg
   :register reg})

(defn- stack-operand [v]
  {:operand :stack
   :operand-type :stack
   :value v})

(defn- pseudo-operand [identifier]
  {:operand :pseudo
   :operand-type :pseudo
   :identifier identifier})

(defn- data-operand [identifier]
  {:operand :data
   :operand-type :data
   :identifier identifier})

(defn- memory-address? [operand]
  (or (contains? #{:data :stack} operand) ;; TODO: remove this check once refactored
      (contains? #{:data :stack} (:operand operand))
      (contains? #{:data :stack} (:operand-type operand))))

;;;; Tacky -> Instructions

(defn- tacky-val->assembly-operand [{:keys [type value]}]
  (condp = type
    :constant (imm-operand value)
    :variable (pseudo-operand value)))

(defn-  tacky-return->assembly [instruction]
  (let [val (:val instruction)
        src (tacky-val->assembly-operand val)
        reg (reg-operand :ax)]
    [(mov-instruction src reg) (ret-instruction)]))

(defn- tacky-unary->assembly [instruction]
  (let [src (tacky-val->assembly-operand (:src instruction))
        dst (tacky-val->assembly-operand (:dst instruction))
        unop (:unary-operator instruction)
        logical-not? (= :logical-not unop)]
    (cond
      logical-not? [(cmp-instruction (imm-operand 0) src)
                    (mov-instruction (imm-operand 0) dst)
                    (setcc-instruction :e dst)]
      :else [(mov-instruction src dst) (unary-instruction unop dst)])))

(def relational-ops
  {:greater-than :g
   :less-than :l
   :equal :e
   :not-equal :ne
   :less-or-equal :le
   :greater-or-equal :ge})

(defn- tacky-binary->assembly [instruction]
  (let [binop (:binary-operator instruction)
        src1 (tacky-val->assembly-operand (:src1 instruction))
        src2 (tacky-val->assembly-operand (:src2 instruction))
        dst (tacky-val->assembly-operand (:dst instruction))
        div? (= binop :div)
        mod? (= binop :mod)
        relational? (contains? relational-ops binop)
        bit-shift? (contains? #{:bit-right-shift :bit-left-shift} binop)]
    (cond
      div? [(mov-instruction src1 (reg-operand :ax))
            (cdq-instruction)
            (idiv-instruction src2)
            (mov-instruction (reg-operand :ax) dst)]
      mod? [(mov-instruction src1 (reg-operand :ax))
            (cdq-instruction)
            (idiv-instruction src2)
            (mov-instruction (reg-operand :dx) dst)]
      relational? [(cmp-instruction src2 src1)
                   (mov-instruction (imm-operand 0) dst)
                   (setcc-instruction (binop relational-ops) dst)]
      bit-shift? [(mov-instruction src1 dst)
                  (mov-instruction src2 (reg-operand :cx))
                  (binary-instruction binop (reg-operand :cl) dst)]
      :else [(mov-instruction src1 dst) (binary-instruction binop src2 dst)])))

(defn- tacky-jump-if-zero->assembly [instruction]
  (let [val (tacky-val->assembly-operand (:val instruction))
        target (:identifier instruction)]
    [(cmp-instruction (imm-operand 0) val)
     (jmpcc-instruction :e target)]))

(defn- tacky-jump-if-not-zero->assembly [instruction]
  (let [val (tacky-val->assembly-operand (:val instruction))
        target (:identifier instruction)]
    [(cmp-instruction (imm-operand 0) val)
     (jmpcc-instruction :ne target)]))

(defn- tacky-jump->assembly [instruction]
  [(jmp-instruction (:identifier instruction))])

(defn- tacky-copy->assembly [instruction]
  (let [src (tacky-val->assembly-operand (:src instruction))
        dst (tacky-val->assembly-operand (:dst instruction))]
    [(mov-instruction src dst)]))

(defn- tacky-label->assembly [instruction]
  [(label-instruction (:identifier instruction))])

(defn- pass-args-in-registers-instructions
  "Caller function stores the arguments in registers.

  Only first 6 arguments are stored in registers. Remaining stored on stack."
  [register-args]
  (let [argument-passing-registers [:di :si :dx :cx :r8 :r9]
        arg-mov-instruction (fn [[reg arg]]
                              (let [operand (tacky-val->assembly-operand arg)]
                                (mov-instruction operand (reg-operand reg))))]
    (->> register-args
         (interleave argument-passing-registers)
         (partition 2)
         (mapv arg-mov-instruction)
         flatten)))

(defn- pass-args-on-stack-instructions
  "Caller function stores the arguments on stack.

  First 6 arguments already stored in registers."
  [stack-args]
  (let [arg-mov-instruction (fn [arg]
                              (let [operand (tacky-val->assembly-operand arg)
                                    operand-type (:type operand)
                                    reg-or-imm? (or (= operand-type :imm) (= operand-type :reg))]
                                (if reg-or-imm?
                                  [(push-instruction operand)]
                                  [(mov-instruction operand (reg-operand :ax))
                                   (push-instruction (reg-operand :ax))])))]
    (->> stack-args
         reverse
         (mapv arg-mov-instruction)
         flatten
         (remove nil?))))

(defn- tacky-fun-call->assembly [{:keys [identifier arguments dst]}]
  (let [[register-args stack-args] (split-at 6 arguments)
        stack-padding (if (odd? (count stack-args)) 8 0)
        fix-stack-alignment-instruction (if (not= stack-padding 0)
                                          [(allocate-stack-instruction stack-padding)]
                                          [])
        bytes-to-remove (+ stack-padding (* 8 (count stack-args)))
        deallocate-arguments-instruction (if (not= bytes-to-remove 0)
                                           [(deallocate-stack-instruction bytes-to-remove)]
                                           [])
        assembly-dst (tacky-val->assembly-operand dst)]
    (->> [fix-stack-alignment-instruction
          (pass-args-in-registers-instructions register-args)
          (pass-args-on-stack-instructions stack-args)
          (call-instruction identifier)
          deallocate-arguments-instruction
          (mov-instruction (reg-operand :ax) assembly-dst)]
         (remove nil?)
         flatten)))

(def tacky->assembly-transformers
  {:unary #'tacky-unary->assembly
   :return #'tacky-return->assembly
   :binary #'tacky-binary->assembly
   :copy #'tacky-copy->assembly
   :jump #'tacky-jump->assembly
   :label #'tacky-label->assembly
   :jump-if-zero #'tacky-jump-if-zero->assembly
   :jump-if-not-zero #'tacky-jump-if-not-zero->assembly
   :fun-call #'tacky-fun-call->assembly})

(defn- tacky-inst->assembly-inst [inst]
  (let [transformer-fn ((:type inst) tacky->assembly-transformers)]
    (transformer-fn inst)))

(defn- find-pseudo-identifiers
  "Returns list of identifiers for pseudo operands.

  Drills into each instruction. Collects identifier from any pseudo operand."
  [instructions]
  (let [pseudo-operand? (fn [instruction path-to-operand]
                          (= :pseudo (get-in instruction [path-to-operand :operand-type])))
        operand-keys-in-instruction [:src :dst :operand]
        instruction->pseudo-values (fn [instruction]
                                     (reduce
                                      (fn [acc path]
                                        (if (pseudo-operand? instruction path)
                                          (conj acc (get-in instruction [path :identifier]))
                                          acc))
                                      []
                                      operand-keys-in-instruction))]
    (->> instructions
         (mapv instruction->pseudo-values)
         flatten
         (remove nil?)
         distinct)))

(defn- pseudo-identifier-to-stack-address
  "Returns a map from pseudo identifiers to stack address in memory.

  Assigns each identifier subsequent lower memory addresses in stack."
  [pseudo-identifiers]
  (reduce
   (fn [acc cur]
     (let [exists? (contains? acc cur)
           v (get acc "current")]
       (if exists?
         acc
         (assoc acc cur (- v 4) "current" (- v 4)))))
   {"current" 0}
   pseudo-identifiers))

(defn- pseudo->data-operand-instruction [ident->symbol instruction]
  (let [pseudo-data-operand? (fn [inst path]
                               (let [operand (get-in inst [path])
                                     operand-type (:operand-type operand)
                                     identifier (:identifier operand)]
                                (and
                                 (= :pseudo operand-type)
                                 (contains? ident->symbol identifier)
                                 (= :static (get-in ident->symbol [identifier :attrs :type])))))
        replace-pseudo-with-data-op (fn [inst path]
                                      (if (pseudo-data-operand? inst path)
                                        (assoc inst path (data-operand (get-in inst [path :identifier])))
                                        inst))]
    (-> instruction
        (replace-pseudo-with-data-op :src)
        (replace-pseudo-with-data-op :dst)
        (replace-pseudo-with-data-op :operand))))

(defn- pseudo->stack-operand-instruction [pseudo-ident->stack-address instruction]
  (let [pseudo-operand? (fn [inst path] (= :pseudo (get-in inst [path :operand-type])))
        replace-pseudo-with-stack-op (fn [inst path]
                                       (if (pseudo-operand? inst path)
                                         (let [v (get-in inst [path :identifier])
                                               sv (get pseudo-ident->stack-address v)]
                                           (assoc inst path (stack-operand sv)))
                                         inst))]
    (-> instruction
        (replace-pseudo-with-stack-op :src)
        (replace-pseudo-with-stack-op :dst)
        (replace-pseudo-with-stack-op :operand))))

(defn- replace-pseudoregisters [instructions ident->symbol]
  (let [instructions-with-data-ops (mapv #(pseudo->data-operand-instruction ident->symbol %) instructions)
        pseudo-identifiers (find-pseudo-identifiers instructions-with-data-ops)
        pseudo-ident->stack-address (pseudo-identifier-to-stack-address pseudo-identifiers)]
    {:max-stack-val  (get pseudo-ident->stack-address "current")
     :instructions (mapv #(pseudo->stack-operand-instruction pseudo-ident->stack-address %) instructions-with-data-ops)}))

(defn- fix-binary-instruction [instruction]
  (let [binop (:binary-operator instruction)
        src (:src instruction)
        dst (:dst instruction)
        mul? (= binop :mul)]
    (if mul?
      (let [dst-memory-address? (memory-address? dst)]
        (if dst-memory-address?
          [(mov-instruction dst (reg-operand :r11))
           (binary-instruction binop src (reg-operand :r11))
           (mov-instruction (reg-operand :r11) dst)]
          instruction))
      (let [both-memory-address? (and
                                  (memory-address? src)
                                  (memory-address? dst))]
        (if both-memory-address?
          [(mov-instruction src (reg-operand :r10))
           (binary-instruction binop (reg-operand :r10) dst)]
          instruction)))))

(defn- fix-mov-instruction [instruction]
  (let [both-memory-address? (and
                              (memory-address? (:src instruction))
                              (memory-address? (:dst instruction)))]
    (if both-memory-address?
      [(mov-instruction (get instruction :src) (reg-operand :r10))
       (mov-instruction (reg-operand :r10) (get instruction :dst))]
      instruction)))

(defn- fix-idiv-instruction [instruction]
  (if (= :imm (get-in instruction [:operand :operand]))
    [(mov-instruction (:operand instruction) (reg-operand :r10))
     (idiv-instruction (reg-operand :r10))]
    instruction))

(defn- fix-cmp-instruction [instruction]
  (let [both-memory-address? (and
                              (memory-address? (:src instruction))
                              (memory-address? (:dst instruction)))
        dst-constant? (= :imm (get-in instruction [:dst :operand]))]
    (cond
      both-memory-address? [(mov-instruction (get instruction :src) (reg-operand :r10))
                            (cmp-instruction (reg-operand :r10) (get instruction :dst))]
      dst-constant? [(mov-instruction (get instruction :dst) (reg-operand :r11))
                     (cmp-instruction (get instruction :src) (reg-operand :r11))]
      :else instruction)))

(def fix-instruction-map
  {:idiv #'fix-idiv-instruction
   :mov #'fix-mov-instruction
   :cmp #'fix-cmp-instruction
   :binary #'fix-binary-instruction})

(defn- fix-instruction [instruction]
  (let [f (or ((:op instruction) fix-instruction-map) #'identity)]
    (f instruction)))

(defn- add-allocate-stack-instruction
  "Adds allocate stack instruction at the start of the function.

  Stack space allocated needs to be a multiple of 16. Rouding up the size of
  stack frame makes it easier to maintain stack alignment during function calls."
  [{instructions :instructions max-stack-val :max-stack-val}]
  (let [v (abs max-stack-val)
        v (cond
              (= (mod v 16) 0) v
              (< v 0) (- v (- 16 (mod v 16)))
              :else (+ v (- 16 (mod v 16))))]
    (cons (allocate-stack-instruction v) instructions)))

(defn- parameters->assembly-instructions
  "Moves parameters from registers and stacks to pseudoregisters.

  First parameters stored in registers.
  Remaining in stack."
  [parameters]
  (let [registers [:di :si :dx :cx :r8 :r9]
        [register-params stack-params] (split-at 6 parameters)
        reg-args-to-pseudo-instructions (mapv (fn [reg param]
                                                [(mov-instruction (reg-operand reg) (pseudo-operand (:identifier param)))])
                                             registers
                                             register-params)
        stack-args-to-pseudo-instruction (into [] (map-indexed (fn [idx param]
                                                                 [(mov-instruction (stack-operand (+ 16 (* 8 idx))) (pseudo-operand (:identifier param)))]) stack-params))]
    (->> [reg-args-to-pseudo-instructions stack-args-to-pseudo-instruction]
         flatten
         (remove nil?))))

(defn- tacky-function->assembly-function
  [{:keys [global? identifier parameters instructions declaration-type]} ident->symbol]
  (let [parameter-instructions (parameters->assembly-instructions parameters)
        body-instructions (->> instructions
                               (keep tacky-inst->assembly-inst)
                               flatten)]
    {:op :function
     :type declaration-type
     :identifier identifier
     :global? global?
     :instructions (->> [parameter-instructions body-instructions]
                        flatten
                        ((fn [is] (replace-pseudoregisters is ident->symbol)))
                        add-allocate-stack-instruction
                        (keep fix-instruction)
                        flatten)}))

(defn- tacky-static-variable->assembly-static-variable
  [{:keys [identifier initial-value global? declaration-type]}]
  {:op :static-variable
   :type declaration-type
   :global? global?
   :identifier identifier
   :initial-value initial-value})

(defn- tacky-top-level->assembly [top-level ident->symbol]
  (condp = (:declaration-type top-level)
    :function (tacky-function->assembly-function top-level ident->symbol)
    :static-variable (tacky-static-variable->assembly-static-variable top-level)
    (exc/compiler-error "Invalid tacky type passed to compiler." top-level)))

(defn- tacky-ast->assembly [{:keys [program ident->symbol]}]
  (mapv #(tacky-top-level->assembly % ident->symbol) program))

(defn generate-assembly [source]
  (-> source
      l/lex
      p/parse
      a/validate
      t/tacky-generate
      tacky-ast->assembly))

(comment

  (generate-assembly
   "
extern int x = 10;

int main (void) {
    int y = x + 100;
    return x;
}

")

  ())
