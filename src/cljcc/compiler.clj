(ns cljcc.compiler
  (:require [cljcc.parser :as p]
            [instaparse.core :as insta]
            [clojure.pprint :as pp]
            [cljcc.tacky :as t]))

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

(def tacky->assembly-transformers
  {:unary #'tacky-unary->assembly
   :return #'tacky-return->assembly})

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

(create-pseudo-value-map
 (find-pseudo-values
  (->> (:instructions (nth tacky-ex 4))
       (map tacky-inst->assembly-inst)
       (flatten))))

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

(pseudo->stack-operand-instruction
 {"tmp.1" 0}
 {:op :mov
  :src {:operand :pseudo :identifier "tmp.1"}})

(defn- replace-pseudoregisters [instructions]
  (let [pseudo-values (find-pseudo-values instructions)
        pseudo-value-map (create-pseudo-value-map pseudo-values)]
    {:max-stack-val  (get pseudo-value-map "current")
     :instructions (map #(pseudo->stack-operand-instruction pseudo-value-map %) instructions)}))

(defn- allocate-scratch-register [instruction]
  (let [mov-both-stack? (fn [i]
                          (and
                           (= (:op i) :mov)
                           (= (get-in i [:src :operand]) :stack)
                           (= (get-in i [:dst :operand]) :stack)))
        allocate-register-fn (fn [i]
                               (if (mov-both-stack? i)
                                 [(mov-instruction (get i :src) (reg-operand :r10))
                                  (mov-instruction (reg-operand :r10) (get i :dst))]
                                 i))]
    (allocate-register-fn instruction)))

(defn- fix-stack-instructions [{instructions :instructions max-stack-val :max-stack-val}]
  (let [allocate-stack-inst (allocate-stack-instruction max-stack-val)
        fixed-instructions (flatten (map allocate-scratch-register instructions))]
    (cons allocate-stack-inst fixed-instructions)))

(defn- assembly-generate-instructions [tacky-instructions]
  (->> tacky-instructions
       (map tacky-inst->assembly-inst)
       (flatten)
       (replace-pseudoregisters)
       (fix-stack-instructions)))

(defn- transform-function [_return-type identifier args body]
  {:op :function
   :identifier (second identifier)
   :args args
   :instructions (assembly-generate-instructions (:instructions body))})

(defn- tacky-ast->assembly [ast]
  (insta/transform
   {:function transform-function}
   ast))

(defn generate-assembly [source]
  (-> source
      p/parse
      t/tacky-generate
      tacky-ast->assembly))

(comment

  (def ex "int main(void) {return -2;}")

  (pp/pprint (-> ex
                 p/parse
                 t/tacky-generate))

  (pp/pprint
   (generate-assembly
    "int main(void) {
return ~(-(~(-1)));
}"))

  ())
