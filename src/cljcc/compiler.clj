(ns cljcc.compiler
  (:require [cljcc.parser :as p]
            [cljcc.tacky :as t]
            [clojure.core.match :refer [match]]
            [cljcc.lexer :as l]
            [cljcc.schema :as schema]
            [cljcc.analyze.core :as a]
            [malli.core :as m]
            [malli.dev.pretty :as pretty]
            [cljcc.util :as util]
            [cljcc.exception :as exc]
            [clojure.string :as str]))

(def registers #{:ax :dx :di :si :r8 :r9 :r10 :r11 :cx :cl :sp})

(def cond-codes #{:e :ne :g :ge :l :le})

;;;; Instructions

(defn- mov-instruction [assembly-type src dst]
  {:op  :mov
   :assembly-type assembly-type
   :src src
   :dst dst})

(defn- movsx-instruction [src dst]
  {:op :movsx
   :src src
   :dst dst})

(defn- unary-instruction [unary-operator assembly-type operand]
  {:op :unary
   :unary-operator unary-operator
   :assembly-type assembly-type
   :operand operand})

(defn- binary-instruction [binop assembly-type src dst]
  {:op :binary
   :binary-operator binop
   :assembly-type assembly-type
   :src src
   :dst dst})

(defn- cmp-instruction [assembly-type src dst]
  {:op :cmp
   :assembly-type assembly-type
   :src src
   :dst dst})

(defn- cdq-instruction [assembly-type]
  {:op :cdq
   :assembly-type assembly-type})

(defn- idiv-instruction [assembly-type operand]
  {:op :idiv
   :assembly-type assembly-type
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
   :value v})

(defn- reg-operand [reg]
  {:pre [(contains? registers reg)]}
  {:operand :reg
   :register reg})

(defn- stack-operand [v]
  {:operand :stack
   :value v})

(defn- pseudo-operand [identifier]
  {:operand :pseudo
   :identifier identifier})

(defn- data-operand [identifier]
  {:operand :data
   :identifier identifier})

;;;; Tacky -> Instructions

(defn- source-type->assembly-type [t]
  (condp = t
    {:type :int} :longword
    {:type :long} :quadword
    (exc/compiler-error "Invalid type for assembly type conversion." t)))

(defn- assembly-type->size [assembly-type]
  (condp = assembly-type
    :longword 4
    :quadword 8
    (exc/compiler-error "Invalid alignment type provided." assembly-type)))

(defn- source-type->alignment [t]
  (condp = t
    {:type :int} 4
    {:type :long} 8
    (exc/compiler-error "Invalid type for alignment conversion." t)))

(defn tacky-val->assembly-type
  "Returns assembly for a tacky value in a given symbol map."
  [{:keys [type value] :as tv} identifier->symbol]
  (condp = type
    :variable (source-type->assembly-type (get-in identifier->symbol [value :type]))
    :constant (condp = (:type value)
                :int :longword
                :long :quadword)
    (exc/compiler-error "Invalid tacky value for assembly type conversion." tv)))

(defn- tacky-val->assembly-operand [{:keys [type value]}]
  (condp = type
    :constant (imm-operand (:value value))
    :variable (pseudo-operand value)))

(defmulti tacky-instruction->assembly-instructions
  (fn [instruction _ident->symbol]
    (:type instruction)))

(defmethod tacky-instruction->assembly-instructions :return
  [{return-value :val} m]
  (let [src (tacky-val->assembly-operand return-value)
        reg (reg-operand :ax)
        src-type (tacky-val->assembly-type return-value m)]
    [(mov-instruction src-type src reg) (ret-instruction)]))

(defmethod tacky-instruction->assembly-instructions :unary
  [{unop :unary-operator
    tacky-src :src
    tacky-dst :dst} m]
  (let [src (tacky-val->assembly-operand tacky-src)
        dst (tacky-val->assembly-operand tacky-dst)
        src-type (tacky-val->assembly-type tacky-src m)
        dst-type (tacky-val->assembly-type tacky-dst m)
        logical-not? (= :logical-not unop)]
    (cond
      logical-not? [(cmp-instruction src-type (imm-operand 0) src)
                    (mov-instruction dst-type (imm-operand 0) dst)
                    (setcc-instruction :e dst)]
      :else [(mov-instruction src-type src dst)
             (unary-instruction unop src-type dst)])))

(def relational-ops
  {:greater-than :g
   :less-than :l
   :equal :e
   :not-equal :ne
   :less-or-equal :le
   :greater-or-equal :ge})

(defmethod tacky-instruction->assembly-instructions :binary
  [{binop :binary-operator
    t-src1 :src1
    t-src2 :src2
    t-dst :dst} m]
  (let [src1 (tacky-val->assembly-operand t-src1)
        src2 (tacky-val->assembly-operand t-src2)
        dst (tacky-val->assembly-operand t-dst)
        src1-type (tacky-val->assembly-type t-src1 m)
        dst-type (tacky-val->assembly-type t-dst m)
        div? (= binop :div)
        mod? (= binop :mod)
        relational? (contains? relational-ops binop)
        bit-shift? (contains? #{:bit-left-shift :bit-right-shift} binop)]
    (cond
      div? [(mov-instruction src1-type src1 (reg-operand :ax))
            (cdq-instruction src1-type)
            (idiv-instruction src1-type src2)
            (mov-instruction src1-type (reg-operand :ax) dst)]
      mod? [(mov-instruction src1-type src1 (reg-operand :ax))
            (cdq-instruction src1-type)
            (idiv-instruction src1-type src2)
            (mov-instruction src1-type (reg-operand :dx) dst)]
      relational? [(cmp-instruction src1-type src2 src1)
                   (mov-instruction dst-type (imm-operand 0) dst)
                   (setcc-instruction (binop relational-ops) dst)]
      bit-shift? [(mov-instruction src1-type src1 dst)
                  (mov-instruction src1-type src2 (reg-operand :cx))
                  (binary-instruction binop src1-type (reg-operand :cl) dst)]
      :else [(mov-instruction src1-type src1 dst)
             (binary-instruction binop src1-type src2 dst)])))

(defmethod tacky-instruction->assembly-instructions :jump-if-zero
  [{cond-val :val
    identifier :identifier} m]
  (let [val (tacky-val->assembly-operand cond-val)
        cond-type (tacky-val->assembly-type cond-val m)]
    [(cmp-instruction cond-type (imm-operand 0) val)
     (jmpcc-instruction :e identifier)]))

(defmethod tacky-instruction->assembly-instructions :jump-if-not-zero
  [{cond-val :val
    identifier :identifier} m]
  (let [val (tacky-val->assembly-operand cond-val)
        cond-type (tacky-val->assembly-type cond-val m)]
    [(cmp-instruction cond-type (imm-operand 0) val)
     (jmpcc-instruction :ne identifier)]))

(defmethod tacky-instruction->assembly-instructions :jump
  [{:keys [identifier]} _m]
  [(jmp-instruction identifier)])

(defmethod tacky-instruction->assembly-instructions :copy
  [{t-src :src
    t-dst :dst} m]
  (let [src (tacky-val->assembly-operand t-src)
        dst (tacky-val->assembly-operand t-dst)
        src-type (tacky-val->assembly-type t-src m)]
    [(mov-instruction src-type src dst)]))

(defmethod tacky-instruction->assembly-instructions :label
  [{:keys [identifier]} _m]
  [(label-instruction identifier)])

(defmethod tacky-instruction->assembly-instructions :sign-extend
  [{t-src :src
    t-dst :dst} _m]
  (let [src (tacky-val->assembly-operand t-src)
        dst (tacky-val->assembly-operand t-dst)]
    [(movsx-instruction src dst)]))

(defmethod tacky-instruction->assembly-instructions :truncate
  [{t-src :src
    t-dst :dst} _m]
  (let [src (tacky-val->assembly-operand t-src)
        dst (tacky-val->assembly-operand t-dst)]
    [(mov-instruction :longword src dst)]))

(defn- pass-args-in-registers-instructions
  "Caller function stores the arguments in registers.

  Only first 6 arguments are stored in registers. Remaining stored on stack."
  [register-args m]
  (let [argument-passing-registers [:di :si :dx :cx :r8 :r9]
        arg-mov-instruction (fn [[reg arg]]
                              (let [operand (tacky-val->assembly-operand arg)
                                    arg-type (tacky-val->assembly-type arg m)]
                                (mov-instruction arg-type operand (reg-operand reg))))]
    (->> register-args
         (interleave argument-passing-registers)
         (partition 2)
         (mapv arg-mov-instruction)
         flatten)))

(defn- pass-args-on-stack-instructions
  "Caller function stores the arguments on stack.

  First 6 arguments already stored in registers."
  [stack-args m]
  (let [arg-mov-instruction (fn [arg]
                              (let [operand (tacky-val->assembly-operand arg)
                                    operand-assembly-type (tacky-val->assembly-type arg m)
                                    operand-type (:operand operand)
                                    _ (prn "********* operand-type" operand-type)
                                    reg-or-imm? (or (= operand-type :imm) (= operand-type :reg))]
                                (if reg-or-imm?
                                  [(push-instruction operand)]
                                  [(mov-instruction operand-assembly-type operand (reg-operand :ax))
                                   (push-instruction (reg-operand :ax))])))]
    (->> stack-args
         reverse
         (mapv arg-mov-instruction)
         flatten
         (remove nil?))))

(defmethod tacky-instruction->assembly-instructions :fun-call
  [{identifier :identifier
    arguments :arguments
    t-dst :dst} m]
  (let [[register-args stack-args] (split-at 6 arguments)
        stack-padding (if (odd? (count stack-args)) 8 0)
        fix-stack-alignment-instruction (if (not= stack-padding 0)
                                          [(binary-instruction :sub :quadword (imm-operand stack-padding) (reg-operand :sp))]
                                          [])
        bytes-to-remove (+ stack-padding (* 8 (count stack-args)))
        deallocate-arguments-instruction (if (not= bytes-to-remove 0)
                                           [(binary-instruction :add :quadword (imm-operand bytes-to-remove) (reg-operand :sp))]
                                           [])
        assembly-dst (tacky-val->assembly-operand t-dst)
        dst-type (tacky-val->assembly-type t-dst m)]
    (->> [fix-stack-alignment-instruction
          (pass-args-in-registers-instructions register-args m)
          (pass-args-on-stack-instructions stack-args m)
          (call-instruction identifier)
          deallocate-arguments-instruction
          (mov-instruction dst-type (reg-operand :ax) assembly-dst)]
         (remove nil?)
         flatten)))

(defn- find-pseudo-identifiers
  "Returns list of identifiers for pseudo operands.

  Drills into each instruction. Collects identifier from any pseudo operand."
  [instructions]
  (let [pseudo-operand? (fn [instruction path-to-operand]
                          (= :pseudo (get-in instruction [path-to-operand :operand])))
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
  [pseudo-identifiers ident->asm-entry]
  (reduce
   (fn [acc identifier]
     (let [exists? (contains? acc identifier)]
       (if exists?
         acc
         (let [current-stack-val  (get acc "current")
               assembly-type (get-in ident->asm-entry [identifier :assembly-type])
               alignment-size (assembly-type->size assembly-type)
               new-offset (util/round-away-from-zero
                           (- current-stack-val alignment-size) alignment-size)]
           (assoc acc
                  identifier new-offset
                  "current" new-offset)))))
   {"current" 0}
   pseudo-identifiers))

(comment

  (pseudo-identifier-to-stack-address
   ["a" "b"]
   {"a" {:assembly-type :longword}
    "b" {:assembly-type :quadword}})

  (pseudo-identifier-to-stack-address
   ["a" "a1" "b" "c" "d" "e"]
   {"a" {:assembly-type :longword}
    "a1" {:assembly-type :longword}
    "b" {:assembly-type :quadword}
    "c" {:assembly-type :quadword}
    "d" {:assembly-type :longword}
    "e" {:assembly-type :quadword}})

  ())

(defn- pseudo->data-operand-instruction [ident->asm-entry instruction]
  (let [pseudo-data-operand? (fn [inst path]
                               (let [operand (get-in inst [path])
                                     operand-type (:operand operand)
                                     identifier (:identifier operand)]
                                 (and
                                  (= :pseudo operand-type)
                                  (contains? ident->asm-entry identifier)
                                  (:static? (get ident->asm-entry identifier)))))
        replace-pseudo-with-data-op (fn [inst path]
                                      (if (pseudo-data-operand? inst path)
                                        (assoc inst path (data-operand (get-in inst [path :identifier])))
                                        inst))]
    (-> instruction
        (replace-pseudo-with-data-op :src)
        (replace-pseudo-with-data-op :dst)
        (replace-pseudo-with-data-op :operand))))

(defn- pseudo->stack-operand-instruction [pseudo-ident->stack-address instruction]
  (let [pseudo-operand? (fn [inst path] (= :pseudo (get-in inst [path :operand])))
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

(defn- replace-pseudoregisters [instructions ident->asm-entry]
  (let [instructions-with-data-ops (mapv #(pseudo->data-operand-instruction ident->asm-entry %) instructions)
        pseudo-identifiers (find-pseudo-identifiers instructions-with-data-ops)
        pseudo-ident->stack-address (pseudo-identifier-to-stack-address pseudo-identifiers ident->asm-entry)]
    {:max-stack-val  (get pseudo-ident->stack-address "current")
     :instructions (mapv #(pseudo->stack-operand-instruction pseudo-ident->stack-address %) instructions-with-data-ops)}))

(defn- fix-binary-instruction [instruction]
  (let [binop (:binary-operator instruction)
        asm-type (:assembly-type instruction)
        src (:src instruction)
        dst (:dst instruction)
        imm-outside-range? (fn [o] (and
                                    (= :imm (:operand o))
                                    (not (util/in-int-range? (:value o)))))]
    (match [instruction]
      [({:assembly-type :quadword
         :binary-operator (:or :add :sub)
         :src {:operand :imm}}
        :guard (comp imm-outside-range? :src))] [(mov-instruction :quadword src (reg-operand :r10))
                                                 (binary-instruction binop asm-type (reg-operand :r10) dst)]
      [{:binary-operator (:or :add :sub)
        :src {:operand (:or :data :stack)}
        :dst {:operand (:or :data :stack)}}] [(mov-instruction asm-type src (reg-operand :r10))
                                              (binary-instruction binop asm-type (reg-operand :r10) dst)]
      [({:assembly-type :quadword
         :binary-operator :mul
         :src {:operand :imm}
         :dst {:operand (:or :data :stack)}}
        :guard (comp imm-outside-range? :src))] [(mov-instruction :quadword src (reg-operand :r10))
                                                 (mov-instruction :quadword dst (reg-operand :r11))
                                                 (binary-instruction binop :quadword (reg-operand :r10) (reg-operand :r11))
                                                 (mov-instruction :quadword (reg-operand :r11) dst)]
      [({:assembly-type :quadword
         :binary-operator :mul
         :src {:operand :imm}}
        :guard (comp imm-outside-range? :src))] [(mov-instruction :quadword src (reg-operand :r10))
                                                 (binary-instruction binop :quadword (reg-operand :r10) dst)]
      [{:binary-operator :mul
        :dst {:operand (:or :data :stack)}}] [(mov-instruction asm-type dst (reg-operand :r11))
                                              (binary-instruction binop asm-type src (reg-operand :r11))
                                              (mov-instruction asm-type (reg-operand :r11) dst)]
      :else instruction)))

(defn- fix-mov-instruction [instruction]
  (let [src (:src instruction)
        dst (:dst instruction)
        assembly-type (:assembly-type instruction)
        imm-outside-range? (fn [o] (and
                                    (= :imm (:operand o))
                                    (not (util/in-int-range? (:value o)))))]
    (match [instruction]
      [{:src {:operand (:or :data :stack)}
        :dst {:operand (:or :data :stack)}}] [(mov-instruction assembly-type src (reg-operand :r10))
                                              (mov-instruction assembly-type (reg-operand :r10) dst)]
      [({:assembly-type :quadword
         :src {:operand :imm}
         :dst {:operand (:or :data :stack)}}
        :guard (comp imm-outside-range? :src))] [(mov-instruction assembly-type src (reg-operand :r10))
                                                 (mov-instruction assembly-type (reg-operand :r10) dst)]
      :else instruction)))

(comment

  ())

(defn- fix-idiv-instruction [instruction]
  (let [assembly-type (:assembly-type instruction)]
    (if (= :imm (get-in instruction [:operand :operand]))
      [(mov-instruction assembly-type (:operand instruction) (reg-operand :r10))
       (idiv-instruction assembly-type (reg-operand :r10))]
      instruction)))

(defn- fix-cmp-instruction [instruction]
  (let [src (:src instruction)
        dst (:dst instruction)
        assembly-type (:assembly-type instruction)
        imm-outside-range? (fn [o] (and
                                    (= :imm (:operand o))
                                    (not (util/in-int-range? (:value o)))))]
    (match [instruction]
      [{:src {:operand (:or :data :stack)}
        :dst {:operand (:or :data :stack)}}] [(mov-instruction assembly-type src (reg-operand :r10))
                                              (cmp-instruction assembly-type (reg-operand :r10) dst)]
      [({:assembly-type :quadword
         :src {:operand :imm}
         :dst {:operand :imm}}
        :guard [(comp imm-outside-range? :src)])] [(mov-instruction :quadword src (reg-operand :r10))
                                                   (mov-instruction :quadword dst (reg-operand :r11))
                                                   (cmp-instruction :quadword (reg-operand :r10) (reg-operand :r11))]
      [({:assembly-type :quadword
         :src {:operand :imm}}
        :guard [(comp imm-outside-range? :src)])] [(mov-instruction :quadword src (reg-operand :r10))
                                                   (cmp-instruction :quadword (reg-operand :r10) dst)]
      [{:dst {:operand :imm}}] [(mov-instruction assembly-type dst (reg-operand :r11))
                                (cmp-instruction assembly-type src (reg-operand :r11))]
      :else instruction)))

(comment

  (fix-cmp-instruction {:op :cmp
                        :assembly-type :quadword
                        :src {:operand :data
                              :value "asd"}
                        :dst {:operand :stack
                              :value 10}})

  (fix-cmp-instruction {:op :cmp
                        :assembly-type :quadword
                        :src {:operand :data
                              :value "asd"}
                        :dst {:operand :reg
                              :register :ax}})

  (fix-cmp-instruction {:op :cmp
                        :assembly-type :quadword
                        :src {:operand :imm
                              :value 10}
                        :dst {:operand :imm
                              :value 10}})

  (fix-cmp-instruction {:op :cmp
                        :assembly-type :quadword
                        :src {:operand :imm
                              :value Long/MAX_VALUE}
                        :dst {:operand :imm
                              :value 10}})

  (fix-cmp-instruction {:op :cmp
                        :assembly-type :quadword
                        :src {:operand :imm
                              :value Long/MAX_VALUE}
                        :dst {:operand :reg
                              :register :ax}})

  (fix-cmp-instruction {:op :cmp
                        :assembly-type :quadword
                        :src {:operand :imm
                              :value 1}
                        :dst {:operand :reg
                              :register :r10}})

  (fix-cmp-instruction {:op :cmp
                        :assembly-type :quadword
                        :src {:operand :reg
                              :register :ax}
                        :dst {:operand :imm
                              :value 10}})

  (fix-cmp-instruction {:op :cmp
                        :assembly-type :longword
                        :src {:operand :reg
                              :register :ax}
                        :dst {:operand :imm
                              :value 10}})

  (fix-cmp-instruction {:op :cmp
                        :assembly-type :longword
                        :src {:operand :reg
                              :register :ax}
                        :dst {:operand :imm
                              :value 10}})

  ())

(defn- fix-movsx-instruction [inst]
  (let [src (:src inst)
        dst (:dst inst)]
    (match [inst]
      [{:src {:operand :imm}
        :dst {:operand (:or :data :stack)}}] [(mov-instruction :longword src (reg-operand :r10))
                                              (movsx-instruction (reg-operand :r10) (reg-operand :r11))
                                              (mov-instruction :quadword (reg-operand :r11) dst)]
      [{:dst {:operand (:or :data :stack)}}] [(movsx-instruction src (reg-operand :r11))
                                              (mov-instruction :quadword (reg-operand :r11) dst)]
      [{:src {:operand :imm}}] [(mov-instruction :longword src (reg-operand :r10))
                                (movsx-instruction (reg-operand :r10) dst)]
      :else inst)))

(comment

  (fix-movsx-instruction {:op :movsx
                          :src {:operand :data
                                :identifier "test"}
                          :dst {:operand :stack
                                :value 10}})

  (fix-movsx-instruction {:op :movsx
                          :src {:operand :imm
                                :value 8}
                          :dst {:operand :stack
                                :value 10}})

  (fix-movsx-instruction {:op :movsx
                          :src {:operand :imm
                                :value 8}
                          :dst {:operand :reg
                                :register :ax}})

  (fix-movsx-instruction {:op :movsx
                          :src {:operand :reg
                                :register :si}
                          :dst {:operand :reg
                                :register :ax}})

  ())

(defn- fix-push-instruction [instruction]
  (let [operand (:operand instruction)
        imm-outside-range? (and (= :imm (:operand operand))
                                (not (util/in-int-range? (:value operand))))]
    (if imm-outside-range?
      [(mov-instruction :quadword operand (reg-operand :r10))
       (push-instruction (reg-operand :r10))]
      instruction)))

(comment

  ())

(def fix-instruction-map
  {:idiv #'fix-idiv-instruction
   :mov #'fix-mov-instruction
   :movsx #'fix-movsx-instruction
   :cmp #'fix-cmp-instruction
   :push #'fix-push-instruction
   :binary #'fix-binary-instruction})

(defn- fix-instruction [instruction _identifier->asm-entry]
  (let [f (or ((:op instruction) fix-instruction-map) #'identity)]
    (f instruction)))

(comment
  (fix-instruction {:op :cmp
                    :assembly-type :longword
                    :src {:operand :imm :value 0}
                    :dst {:operand :imm :value 5}} {}))

(defn- add-allocate-stack-instruction
  "Adds allocate stack instruction at the start of the function.

  Stack space allocated needs to be a multiple of 16. Rouding up the size of
  stack frame makes it easier to maintain stack alignment during function calls."
  [{instructions :instructions max-stack-val :max-stack-val}]
  (let [v (util/round-away-from-zero (abs max-stack-val) 16)]
    (cons
     (binary-instruction :sub :quadword (imm-operand v) (reg-operand :sp))
     instructions)))

(defn- parameters->assembly-instructions
  "Moves parameters from registers and stacks to pseudoregisters.

  First parameters stored in registers.
  Remaining in stack."
  [parameters function-type]
  (let [registers [:di :si :dx :cx :r8 :r9]
        [register-params stack-params] (split-at 6 parameters)
        [register-param-types stack-param-types] (split-at 6 (:parameter-types function-type))
        reg-args-to-pseudo-instructions (mapv (fn [reg param param-type]
                                                [(mov-instruction
                                                  (source-type->assembly-type param-type)
                                                  (reg-operand reg)
                                                  (pseudo-operand param))])
                                              registers
                                              register-params
                                              register-param-types)
        stack-args-to-pseudo-instruction (into [] (apply map (fn [idx param param-type]
                                                               [(mov-instruction
                                                                 (source-type->assembly-type param-type)
                                                                 (stack-operand (+ 16 (* 8 idx)))
                                                                 (pseudo-operand param))])
                                                         (range)
                                                         [stack-params
                                                          stack-param-types]))]
    (->> [reg-args-to-pseudo-instructions stack-args-to-pseudo-instruction]
         flatten
         (remove nil?))))

(defn- tacky-function->assembly-function
  [{:keys [global? identifier parameters instructions]} ident->symbol]
  (let [function-type (:type (get ident->symbol identifier))
        parameter-instructions (parameters->assembly-instructions parameters function-type)
        body-instructions (->> instructions
                               (keep #(tacky-instruction->assembly-instructions % ident->symbol))
                               flatten)]
    {:op :function
     :identifier identifier
     :global? global?
     :instructions (vec (flatten [parameter-instructions body-instructions]))}))

(defn fix-assembly-function
  "Fixes assembly functions.

  Replaces pseudoregisters, fix instruction."
  [assembly-f identifier->asm-entry]
  (let [instructions (:instructions assembly-f)]
    (assoc assembly-f
           :instructions
           (->> instructions
                ((fn [insts] (replace-pseudoregisters insts identifier->asm-entry)))
                add-allocate-stack-instruction
                (keep #(fix-instruction % identifier->asm-entry))
                flatten
                vec))))

(defn- tacky-static-variable->assembly-static-variable
  [{:keys [identifier initial global? variable-type]}]
  {:op :static-variable
   :global? global?
   :alignment (source-type->alignment variable-type)
   :identifier identifier
   :initial initial})

(defn backend-symbol-table [ident->symbol]
  (let [function? (fn [t] (= :function (:type t)))
        static? (fn [attr] (boolean (= :static (:type attr))))
        f (fn [{:keys [type attribute]}]
            (if (function? type)
              {:type :fun-entry
               :defined? (:defined? attribute)}
              {:type :obj-entry
               :static? (static? attribute)
               :assembly-type (source-type->assembly-type type)}))]
    (update-vals ident->symbol f)))

(defn assembly [{tacky-program :program
                 ident->symbol :ident->symbol}]
  (let [assembly-static-variables (->> tacky-program
                                       (filterv #(= :static-variable (:declaration-type %)))
                                       (mapv tacky-static-variable->assembly-static-variable))
        assembly-functions (->> tacky-program
                                (filterv #(= :function (:declaration-type %)))
                                (mapv #(tacky-function->assembly-function % ident->symbol)))
        _ (prn assembly-functions)
        backend-symbol-table (backend-symbol-table ident->symbol)
        fixed-assembly-functions (mapv #(fix-assembly-function % backend-symbol-table) assembly-functions)
        program (vec (flatten [assembly-static-variables fixed-assembly-functions]))
        ; _ (m/coerce schema/AssemblyProgram program)
        ; _ (m/coerce schema/BackendSymbolMap backend-symbol-table)
        ]
    {:program program
     :backend-symbol-table backend-symbol-table}))

(defn assembly-from-src [src]
  (-> src
      l/lex
      p/parse
      a/validate
      t/tacky-generate
      assembly))

(comment

  (def file-path "./test-programs/example.c")

  (def input (slurp file-path))

  input

  (assembly-from-src input)

  (pretty/explain
   schema/AssemblyProgram
   (:program (assembly-from-src input)))

  (pretty/explain
   schema/BackendSymbolMap
   (:backend-symbol-table (assembly-from-src input)))

  ())
