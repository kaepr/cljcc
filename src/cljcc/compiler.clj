(ns cljcc.compiler
  (:require [cljcc.parser :as p]
            [clojure.pprint :as pp]
            [cljcc.tacky :as t]
            [cljcc.lexer :as l]))

(def registers #{:ax :dx :r10 :r11})

;;;; Instructions

(defn- mov-instruction [src dst]
  {:op  :mov
   :src src
   :dst dst})

(defn- ret-instruction []
  {:op :ret})

(defn- allocate-stack-instruction [v]
  {:op :allocate-stack
   :value v})

(defn- unary-instruction [unary-operator operand]
  {:op :unary
   :unary-operator unary-operator
   :operand operand})

(defn- binary-instruction [binop src dst]
  {:op :binary
   :binary-operator binop
   :src src
   :dst dst})

(defn- cdq-instruction []
  {:op :cdq})

(defn- idiv-instruction [operand]
  {:op :idiv
   :operand operand})

;;;; Operands

(defn- imm-operand [v]
  {:operand :imm
   :value v})

(defn- reg-operand [reg]
  {:operand :reg
   :register reg})

(defn- stack-operand [v]
  {:operand :stack
   :value v})

(defn- pseudo-operand [identifier]
  {:operand :pseudo
   :identifier identifier})

;;;; Tacky -> Instructions

(defn- tacky-val->assembly-operand [val]
  (let [type (:type val)
        v (:value val)]
    (condp = type
      :constant (imm-operand v)
      :variable (pseudo-operand v))))

(defn-  tacky-return->assembly [instruction]
  (let [val (:val instruction)
        src (tacky-val->assembly-operand val)
        reg (reg-operand :ax)]
    [(mov-instruction src reg) (ret-instruction)]))

(defn- tacky-unary->assembly [instruction]
  (let [src (tacky-val->assembly-operand (:src instruction))
        dst (tacky-val->assembly-operand (:dst instruction))
        unop (:unary-operator instruction)]
    [(mov-instruction src dst) (unary-instruction unop dst)]))

(defn- tacky-binary->assembly [instruction]
  (let [binop (:binary-operator instruction)
        src1 (tacky-val->assembly-operand (:src1 instruction))
        src2 (tacky-val->assembly-operand (:src2 instruction))
        dst (tacky-val->assembly-operand (:dst instruction))
        div? (= binop :div)
        mod? (= binop :mod)
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
      bit-shift? [(mov-instruction src1 dst)
                  (mov-instruction src2 (reg-operand :cx))
                  (binary-instruction binop (reg-operand :cl) dst)]
      :else [(mov-instruction src1 dst) (binary-instruction binop src2 dst)])))

(def tacky->assembly-transformers
  {:unary #'tacky-unary->assembly
   :return #'tacky-return->assembly
   :binary #'tacky-binary->assembly})

(defn- tacky-inst->assembly-inst [inst]
  (let [transformer-fn ((:type inst) tacky->assembly-transformers)]
    (transformer-fn inst)))

(declare tacky-ex)

(defn- find-pseudo-values [instructions]
  (distinct
   (remove
    nil?
    (reduce (fn [pseudo-acc inst]
              (let [paths [:src :dst :operand]
                    values (reduce (fn [acc path]
                                     (if (get-in inst [path :operand])
                                       (conj acc (get-in inst [path :identifier]))
                                       acc)) [] paths)]
                (concat pseudo-acc values))) [] instructions))))

(defn- create-pseudo-value-map [pseudo-values]
  (reduce
   (fn [acc cur]
     (let [exists? (contains? acc cur)
           v (get acc "current")]
       (if exists?
         acc
         (assoc acc cur (- v 4) "current" (- v 4)))))
   {"current" 0}
   pseudo-values))

(defn- pseudo->stack-operand-instruction [pvs instruction]
  (let [pseudo? (fn [inst path]
                  (let [v (get-in inst [path :operand])]
                    (if v
                      (= :pseudo v)
                      false)))
        replace-pseudo (fn [inst path]
                         (if (pseudo? inst path)
                           (let [v (get-in inst [path :identifier])
                                 sv (get pvs v)]
                             (assoc inst path (stack-operand sv)))
                           inst))]
    (-> instruction
        (replace-pseudo :src)
        (replace-pseudo :dst)
        (replace-pseudo :operand))))

(defn- replace-pseudoregisters [instructions]
  (let [pseudo-values (find-pseudo-values instructions)
        pseudo-value-map (create-pseudo-value-map pseudo-values)]
    {:max-stack-val  (get pseudo-value-map "current")
     :instructions (map #(pseudo->stack-operand-instruction pseudo-value-map %) instructions)}))

(defn- fix-binary-instruction [instruction]
  (let [binop (:binary-operator instruction)
        src (:src instruction)
        dst (:dst instruction)
        mul? (= binop :mul)]
    (if mul?
      (let [dst-memory-address? (= :stack (:operand dst))]
        (if dst-memory-address?
          [(mov-instruction dst (reg-operand :r11))
           (binary-instruction binop src (reg-operand :r11))
           (mov-instruction (reg-operand :r11) dst)]
          instruction))
      (let [both-memory-address? (and
                                  (= :stack (:operand src))
                                  (= :stack (:operand dst)))]
        (if both-memory-address?
          [(mov-instruction src (reg-operand :r10))
           (binary-instruction binop (reg-operand :r10) dst)]
          instruction)))))

(defn- fix-mov-instruction [instruction]
  (let [both-memory-address? (and
                              (= :stack (get-in instruction [:src :operand]))
                              (= :stack (get-in instruction [:dst :operand])))]
    (if both-memory-address?
      [(mov-instruction (get instruction :src) (reg-operand :r10))
       (mov-instruction (reg-operand :r10) (get instruction :dst))]
      instruction)))

(defn- fix-idiv-instruction [instruction]
  (if (= :imm (get-in instruction [:operand :operand]))
    [(mov-instruction (:operand instruction) (reg-operand :r10))
     (idiv-instruction (reg-operand :r10))]
    instruction))

(def fix-instruction-map
  {:idiv #'fix-idiv-instruction
   :mov #'fix-mov-instruction
   :binary #'fix-binary-instruction})

(defn- fix-instruction [instruction]
  (let [f (or ((:op instruction) fix-instruction-map) #'identity)]
    (f instruction)))

(defn- add-allocate-stack-instruction [{instructions :instructions max-stack-val :max-stack-val}]
  (cons (allocate-stack-instruction max-stack-val) instructions))

(defn- assembly-generate-instructions [tacky-instructions]
  (->> tacky-instructions
       (map tacky-inst->assembly-inst)
       flatten
       replace-pseudoregisters
       add-allocate-stack-instruction
       (map fix-instruction)
       flatten))

(defn- transform-function [fn-ast]
  {:op (:type fn-ast)
   :identifier (:identifier fn-ast)
   :parameters (:parameters fn-ast)
   :instructions (assembly-generate-instructions (:instructions fn-ast))})

(defn- tacky-ast->assembly [ast]
  (map transform-function ast))

(defn generate-assembly [source]
  (-> source
      l/lex
      p/parse
      t/tacky-generate
      tacky-ast->assembly))

(comment

  (def ex "int main(void){return 2;}")

  (pp/pprint (-> ex
                 generate-assembly))

  (pp/pprint
   (generate-assembly
    "int main(void) {
return 1 + 2;
}"))

  ())
