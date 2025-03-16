(ns cljcc.emit
  (:require
   [cljcc.util :refer [get-os]]
   [cljcc.compiler :as c]
   [cljcc.core.format :refer [safe-format]]
   [clojure.string :as str]
   [cljcc.core.exception :as exc]))

(defn- handle-label [identifier]
  (condp = (get-os)
    :mac (str "L" identifier)
    :linux (str ".L" identifier)
    (throw (ex-info "Error in generating label." {}))))

(defn- handle-symbol-name [name]
  (if (= :mac (get-os))
    (str "_" name)
    name))

(defn- handle-current-translation-unit [name ident->asm-entry]
  (if (= :mac (get-os))
    (handle-symbol-name name)
    (if (get-in ident->asm-entry [name :defined?])
      name
      (str name "@PLT"))))

;;;; Operand Emit

(defn- imm-opernad-emit [operand _opts]
  (safe-format "$%d" (:value operand)))

(defn- stack-operand-emit [operand _opts]
  (safe-format "%d(%%rbp)" (:value operand)))

(defn- data-operand-emit [operand _opts]
  (safe-format "%s(%%rip)" (handle-symbol-name (:identifier operand))))

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
                                      :1-byte "%cl"}

                                 :sp {:8-byte "%rsp"
                                      :4-byte "%rsp"
                                      :1-byte "%rsp"}}]
    (if-let [out (get-in register->width->output [register register-width])]
      out
      (exc/emit-error "Invalid register and width" {:operand operand
                                                    :opts register-width}))))

(def operand-emitters
  "Map of assembly operands to operand emitters."
  {:imm #'imm-opernad-emit
   :reg #'register-operand
   :data #'data-operand-emit
   :stack #'stack-operand-emit})

(defn- operand-emit
  ([operand]
   (operand-emit operand {}))
  ([operand opts]
   (if-let [[_ operand-emit-fn] (find operand-emitters (:operand operand))]
     (operand-emit-fn operand opts)
     (exc/emit-error "Invalid operand" {:operand operand}))))

;;;; Instruction Emit

(defn- assembly-type->instruction-suffix [atype]
  (condp = atype
    :longword "l"
    :quadword "q"))

(defn- assembly-type->operand-size [atype]
  (condp = atype
    :longword :4-byte
    :quadword :8-byte))

(defn- mov-instruction-emit [instruction]
  (let [atype (:assembly-type instruction)
        opts {:register-width (assembly-type->operand-size atype)}
        src (operand-emit (:src instruction) opts)
        dst (operand-emit (:dst instruction) opts)
        suffix (assembly-type->instruction-suffix atype)]
    [(format "    %s%s        %s, %s" "mov" suffix src dst)]))

(defn- movsx-instruction-emit [instruction]
  (let [src (operand-emit (:src instruction) {:register-width :4-byte})
        dst (operand-emit (:dst instruction) {:register-width :8-byte})]
    [(format "    %s        %s, %s" "movslq" src dst)]))

(defn- cmp-instruction-emit [instruction]
  (let [atype (:assembly-type instruction)
        opts {:register-width (assembly-type->operand-size atype)}
        src (operand-emit (:src instruction) opts)
        dst (operand-emit (:dst instruction) opts)
        suffix (assembly-type->instruction-suffix atype)]
    [(format "    %s%s        %s, %s" "cmp" suffix src dst)]))

(defn- jmp-instruction-emit [instruction]
  [(format "    jmp        %s" (handle-label (:identifier instruction)))])

(defn- jmpcc-instruction-emit [instruction]
  (let [cc (name (:cond-code instruction))
        label (handle-label (:identifier instruction))]
    [(format "    j%s         %s" cc label)]))

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
  (let [atype (:assembly-type instruction)
        opts {:register-width (assembly-type->operand-size atype)}
        operand (operand-emit (:operand instruction) opts)
        suffix (assembly-type->instruction-suffix (:assembly-type instruction))
        assembly-operator (condp = (:unary-operator instruction)
                            :bit-not "not"
                            :negate "neg"
                            (throw (AssertionError. (str "Invalid unary operator: " instruction))))]
    [(format "    %s%s        %s" assembly-operator suffix operand)]))

(defn- binary-instruction-emit [instruction]
  (let [atype (:assembly-type instruction)
        opts {:register-width (assembly-type->operand-size atype)}
        src (operand-emit (:src instruction) opts)
        dst (operand-emit (:dst instruction) opts)
        suffix (assembly-type->instruction-suffix (:assembly-type instruction))
        binop (:binary-operator instruction)
        binop-operator (condp = binop
                         :add "add"
                         :sub "sub"
                         :mul "imul"
                         :bit-and "and"
                         :bit-xor "xor"
                         :bit-or "or"
                         :bit-left-shift "sal"
                         :bit-right-shift "sar"
                         (throw (AssertionError. (str "Invalid binary operator: " instruction))))]
    [(format "    %s%s        %s, %s" binop-operator suffix src dst)]))

(defn- cdq-instruction-emit [{:keys [assembly-type] :as _instruction}]
  (let [opcode (if (= :longword assembly-type)
                 "cdq"
                 "cqo")]
    [(format "    %s" opcode)]))

(defn- idiv-instruction-emit [instruction]
  (let [atype (:assembly-type instruction)
        opts {:register-width (assembly-type->operand-size atype)}
        op (operand-emit (:operand instruction) opts)
        suffix (assembly-type->instruction-suffix (:assembly-type instruction))]
    [(format "    idiv%s       %s" suffix op)]))

(defn- div-instruction-emit [instruction]
  (let [atype (:assembly-type instruction)
        opts {:register-width (assembly-type->operand-size atype)}
        op (operand-emit (:operand instruction) opts)
        suffix (assembly-type->instruction-suffix (:assembly-type instruction))]
    [(format "     div%s       %s" suffix op)]))

(defn- push-instruction-emit [instruction]
  [(format "    pushq       %s" (operand-emit (:operand instruction) {:register-width :8-byte}))])

(defn- call-instruction-emit [instruction m]
  [(format "    call        %s" (handle-current-translation-unit (:identifier instruction) m))])

(def instruction-emitters
  "Map of assembly instructions to function emitters."
  {:mov #'mov-instruction-emit
   :movsx #'movsx-instruction-emit
   :ret #'ret-instruction-emit
   :binary #'binary-instruction-emit
   :cdq #'cdq-instruction-emit
   :idiv #'idiv-instruction-emit
   :div #'div-instruction-emit
   :unary #'unary-instruction-emit
   :setcc #'setcc-instruction-emit
   :jmp #'jmp-instruction-emit
   :jmpcc #'jmpcc-instruction-emit
   :label #'label-instruction-emit
   :cmp #'cmp-instruction-emit
   :push #'push-instruction-emit
   :call #'call-instruction-emit})

(defn instruction-emit [instruction ident->asm-entry]
  (if-let [[op-type instruction-emit-fn] (find instruction-emitters (:op instruction))]
    (if (= :call op-type)
      (instruction-emit-fn instruction ident->asm-entry)
      (instruction-emit-fn instruction))
    (throw (AssertionError. (str "Invalid instruction: " instruction)))))

(defn function-definition-emit [{:keys [identifier instructions global?]} ident->asm-entry]
  (let [name (handle-symbol-name identifier)
        globl (if global?
                (format "    .globl %s", name)
                "")
        name-line (format "%s:" name)
        instructions (mapv #(instruction-emit % ident->asm-entry) instructions)]
    (->> [globl
          "    .text"
          name-line
          "    pushq       %rbp"
          "    movq        %rsp, %rbp"
          instructions
          "\n"]
         flatten
         (filterv not-empty))))

(defn- static-variable-definition-emit [{:keys [identifier global? alignment initial]} _ident->asm-entry]
  (let [name (handle-symbol-name identifier)
        value-type (:type (:static-init initial))
        value (:value (:static-init initial))
        globl (if global?
                (format "    .globl %s" name)
                "")
        data-or-bss (if (zero? value)
                      "    .bss"
                      "    .data")
        initializer-directive (cond
                                (or (= :int-init value-type)
                                    (= :uint-init value-type)) (if (zero? value)
                                                                 "    .zero 4"
                                                                 (format "    .long %d" value))
                                (or (= :long-init value-type)
                                    (= :ulong-init value-type)) (if (zero? value)
                                                                  "    .zero 8"
                                                                  (format "    .quad %d" value)))]
    (filterv not-empty [globl
                        data-or-bss
                        (format "    .balign %d" alignment)
                        (format "%s:" name)
                        initializer-directive
                        "\n"])))

(def emitters-top-level
  "Map of assembly top level constructs to their emitters."
  {:function #'function-definition-emit
   :static-variable #'static-variable-definition-emit})

(defn emit-top-level [ast ident->asm-entry]
  (if-let [[_ emit-fn] (find emitters-top-level (:op ast))]
    (emit-fn ast ident->asm-entry)
    (exc/emit-error "Invalid ast." ast)))

(def linux-assembly-end ".section .note.GNU-stack,\"\",@progbits\n")

(defn emit [{:keys [program backend-symbol-table]}]
  (let [handle-os (fn [ast]
                    (if (= :linux (get-os))
                      (conj (conj (conj (vec ast) linux-assembly-end) "\n"))
                      ast))]
    (->> program
         (mapv #(emit-top-level % backend-symbol-table))
         concat
         flatten
         handle-os
         (str/join "\n"))))

(comment

  (def file-path "./test-programs/example.c")

  (slurp "./test-programs/example.c")

  (-> file-path
      slurp
      c/assembly-from-src)

  (str/split-lines
   (-> file-path
       slurp
       c/assembly-from-src
       emit))

  (spit
   "./test-programs/example.s"
   (-> file-path
       slurp
       c/assembly-from-src
       emit))

  ())
