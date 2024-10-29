(ns cljcc.emit
  (:require [cljcc.parser :as p]
            [cljcc.util :refer [get-os]]
            [cljcc.compiler :as c]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [cljcc.symbols :as symbols]))

(defn- handle-label [identifier]
  (condp = (get-os)
    :mac (str "L" identifier)
    :linux (str ".L" identifier)
    (throw (ex-info "Error in generating label." {}))))


(defn- handle-function-name [name]
  (if (= :mac (get-os))
    (str "_" name)
    name))

(defn- handle-current-translation-unit [name]
  (if (= :mac (get-os))
    (handle-function-name name)
    (if (contains? @symbols/symbols name)
      name
      (str name "@PLT"))))

;;;; Operand Emit

(defn- imm-opernad-emit [operand _opts]
  (format "$%d" (:value operand)))

(defn- stack-operand-emit [operand _opts]
  (format "%d(%%rbp)" (:value operand)))

(defn- register-operand [{:keys [register] :as operand} {register-width :register-width :or {register-width :4-byte}}]
  (let [register->width->output {:ax {:8-byte "%rax"
                                      :4-byte "%eax"
                                      :1-byte "%al"}

                                 :dx {:8-byte "%rdx"
                                      :4-byte "%edx"
                                      :1-byte "%dl"}

                                 :cx {:8-byte "%rcx"
                                      :4-byte "%ecx"
                                      :1-byte "%cl"}

                                 :di {:8-byte "%rdi"
                                      :4-byte "%edi"
                                      :1-byte "%dil"}

                                 :si {:8-byte "%rsi"
                                      :4-byte "%esi"
                                      :1-byte "%sil"}

                                 :r8 {:8-byte "%r8"
                                      :4-byte "%r8d"
                                      :1-byte "%r8b"}

                                 :r9 {:8-byte "%r9"
                                      :4-byte "%r9d"
                                      :1-byte "%r9b"}

                                 :r10 {:8-byte "%r10"
                                       :4-byte "%r10d"
                                       :1-byte "%r10b"}

                                 :r11 {:8-byte "%r11"
                                       :4-byte "%r11d"
                                       :1-byte "%r11b"}

                                 :cl {:4-byte "%cl"
                                      :1-byte "%cl"}}]
    (if-let [out (get-in register->width->output [register register-width])]
      out
      (throw (AssertionError. (str "Invalid register operand register width " operand " " register-width))))))

(def operand-emitters
  "Map of assembly operands to operand emitters."
  {:imm #'imm-opernad-emit
   :reg #'register-operand
   :stack #'stack-operand-emit})

(defn- operand-emit
  ([operand]
   (operand-emit operand {}))
  ([operand opts]
   (if-let [[_ operand-emit-fn] (find operand-emitters (:operand operand))]
     (operand-emit-fn operand opts)
     (throw (AssertionError. (str "Invalid operand: " operand))))))

;;;; Instruction Emit

(defn- mov-instruction-emit [instruction]
  (let [src (operand-emit (:src instruction))
        dst (operand-emit (:dst instruction))]
    [(format "    %s        %s, %s" "movl" src dst)]))

(defn- cmp-instruction-emit [instruction]
  (let [src (operand-emit (:src instruction))
        dst (operand-emit (:dst instruction))]
    [(format "    %s       %s, %s" "cmpl" src dst)]))

(defn- jmp-instruction-emit [instruction]
  [(format "    jmp        %s" (handle-label (:identifier instruction)))])

(defn- jmpcc-instruction-emit [instruction]
  (let [cc (name (:cond-code instruction))
        label (handle-label (:identifier instruction))]
    [(format "    j%s        %s" cc label)]))

(defn- setcc-instruction-emit [instruction]
  (let [cc (name (:cond-code instruction))
        operand (operand-emit (:operand instruction) {:register-width :1-byte})]
    [(format "    set%s        %s" cc operand)]))

(defn- label-instruction-emit [instruction]
  [(format "    %s:" (handle-label (:identifier instruction)))])

(defn- ret-instruction-emit [_instruction]
  ["    movq        %rbp, %rsp"
   "    popq        %rbp"
   "    ret"])

(defn- unary-instruction-emit [instruction]
  (let [operand (operand-emit (:operand instruction))
        assembly-operator (condp = (:unary-operator instruction)
                            :bit-not "notl"
                            :negate "negl"
                            (throw (AssertionError. (str "Invalid unary operator: " instruction))))]
    [(format "    %s        %s" assembly-operator operand)]))

(defn- binary-instruction-emit [instruction]
  (let [src (operand-emit (:src instruction))
        dst (operand-emit (:dst instruction))
        binop (:binary-operator instruction)
        binop-operator (condp = binop
                         :add "addl"
                         :sub "subl"
                         :mul "imull"
                         :bit-and "andl"
                         :bit-xor "xorl"
                         :bit-or "orl"
                         :bit-left-shift "sall"
                         :bit-right-shift "sarl"
                         (throw (AssertionError. (str "Invalid binary operator: " instruction))))]
    [(format "    %s        %s, %s" binop-operator src dst)]))

(defn- cdq-instruction-emit [_instruction]
  ["    cdq"])

(defn- idiv-instruction-emit [instruction]
  [(format "    idivl       %s" (operand-emit (:operand instruction)))])

(defn- allocate-stack-instruction-emit [instruction]
  [(format "    subq        $%d, %%rsp" (:value instruction))])

(defn- deallocate-stack-instruction-emit [instruction]
  [(format "    addq        $%d, %%rsp" (:value instruction))])

(defn- push-instruction-emit [instruction]
  [(format "    pushq       %s" (operand-emit (:operand instruction) {:register-width :8-byte}))])

(defn- call-instruction-emit [instruction]
  [(format "    call        %s" (handle-current-translation-unit (:identifier instruction)))])

(def instruction-emitters
  "Map of assembly instructions to function emitters."
  {:mov #'mov-instruction-emit
   :ret #'ret-instruction-emit
   :binary #'binary-instruction-emit
   :cdq #'cdq-instruction-emit
   :idiv #'idiv-instruction-emit
   :unary #'unary-instruction-emit
   :setcc #'setcc-instruction-emit
   :jmp #'jmp-instruction-emit
   :jmpcc #'jmpcc-instruction-emit
   :label #'label-instruction-emit
   :cmp #'cmp-instruction-emit
   :allocate-stack #'allocate-stack-instruction-emit
   :deallocate-stack #'deallocate-stack-instruction-emit
   :push #'push-instruction-emit
   :call #'call-instruction-emit})

(defn instruction-emit [instruction]
  (if-let [[_ instruction-emit-fn] (find instruction-emitters (:op instruction))]
    (instruction-emit-fn instruction)
    (throw (AssertionError. (str "Invalid instruction: " instruction)))))

(defn function-definition-emit [f]
  (let [name (handle-function-name (:identifier f))
        globl (format "    .globl %s", name)
        name-line (format "%s:" name)
        instructions (map instruction-emit (:instructions f))]
    (flatten [globl
              name-line
              "    pushq       %rbp"
              "    movq        %rsp, %rbp"
              instructions])))

(def emitters-top-level
  "Map of assembly top level constructs to their emitters."
  {:declaration #'function-definition-emit})

(defn emit-top-level [assembly-ast]
  (if-let [[_ emit-fn] (find emitters-top-level (:op assembly-ast))]
    (emit-fn assembly-ast)
    (throw (AssertionError. (str "Invalid ast: " assembly-ast)))))

(def linux-assembly-end ".section .note.GNU-stack,\"\",@progbits")

(defn emit [ast]
  (let [handle-os (fn [ast]
                    (if (= :linux (get-os))
                      (conj (conj (vec ast) linux-assembly-end) "\n")
                      (conj ast "\n")))]
    (->> ast
         (map emit-top-level)
         concat
         flatten
         handle-os
         (str/join "\n"))))

(comment

   (emit
    (c/generate-assembly
     "int main(void) {
     return ~(-(~(-1)));
    }"))

  ())
