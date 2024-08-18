(ns cljcc.emit
  (:require [cljcc.parser :as p]
            [cljcc.util :refer [get-os]]
            [cljcc.compiler :as c]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

;;;; Operand Emit

(defn- imm-opernad-emit [operand]
  (format "$%d" (:value operand)))

(defn- stack-operand-emit [operand]
  (format "%d(%%rbp)" (:value operand)))

(defn- register-operand [operand]
  (condp = (:register operand)
    :ax "%eax"
    :dx "%edx"
    :r10 "%r10d"
    :r11 "%r11d"
    :cx "%ecx"
    :cl "%cl"
    (throw (AssertionError. (str "Invalid register operand: " operand)))))

(def operand-emitters
  "Map of assembly operands to operand emitters."
  {:imm #'imm-opernad-emit
   :reg #'register-operand
   :stack #'stack-operand-emit})

(defn- operand-emit [operand]
  (if-let [[_ operand-emit-fn] (find operand-emitters (:operand operand))]
    (operand-emit-fn operand)
    (throw (AssertionError. (str "Invalid operand: " operand)))))

;;;; Instruction Emit

(defn- mov-instruction-emit [instruction]
  (let [src (operand-emit (:src instruction))
        dst (operand-emit (:dst instruction))]
    [(format "    %s        %s, %s" "movl" src dst)]))

(defn- ret-instruction-emit [_instruction]
  ["    movq        %rbp, %rsp"
   "    popq        %rbp"
   "    ret"])

(defn- unary-instruction-emit [instruction]
  (let [operand (operand-emit (:operand instruction))
        assembly-operator (condp = (:unary-operator instruction)
                            :complement "notl"
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

(def instruction-emitters
  "Map of assembly instructions to function emitters."
  {:mov #'mov-instruction-emit
   :ret #'ret-instruction-emit
   :binary #'binary-instruction-emit
   :cdq #'cdq-instruction-emit
   :idiv #'idiv-instruction-emit
   :unary #'unary-instruction-emit
   :allocate-stack #'allocate-stack-instruction-emit})

(defn instruction-emit [instruction]
  (if-let [[_ instruction-emit-fn] (find instruction-emitters (:op instruction))]
    (instruction-emit-fn instruction)
    (throw (AssertionError. (str "Invalid instruction: " instruction)))))

(defn- handle-function-name [name]
  (if (= :mac (get-os))
    (str "_" name)
    name))

(defn function-emit [f]
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
  {:function #'function-emit})

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

  (def ex "int main(void) {return 2;}")

  (mov-instruction-emit
   {:op :mov
    :src {:operand :imm :value 1}
    :dst {:operand :stack :value -4}})

  (c/generate-assembly
   "int main(void) {
     return ~(-(~(-1)));
    }")

  (pp/pprint
   (c/generate-assembly
    "int main(void) {
     return ~(-(~(-1)));
    }"))

  (println
   (emit
    (c/generate-assembly
     "int main(void) {
       return 6 / 3 / 2;
    }")))

  (println
   (emit
    (c/generate-assembly
     "int main(void) {
       return 6;
    }")))

  (-> ex
      p/parse)

  ())
