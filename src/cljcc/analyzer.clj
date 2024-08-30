(ns cljcc.analyzer
  (:require [cljcc.lexer :as l]
            [clojure.pprint :as pp]
            [cljcc.util :as u]
            [cljcc.parser :as p]))

(defn- unique-identifier
  ([] (unique-identifier "analyzer"))
  ([identifier] (u/create-identifier! identifier)))

(declare resolve-block)

(defn- resolve-exp [e mp]
  (condp = (:exp-type e)
    :constant-exp e
    :variable-exp (if (contains? mp (:identifier e))
                    (p/variable-exp-node (:name (get mp (:identifier e))))
                    (throw (ex-info "Undeclared variable seen." {:variable e})))
    :assignment-exp (let [left (:left e)
                          right (:right e)
                          op (:assignment-operator e)
                          left-var? (= :variable-exp (:exp-type left))]
                      (if left-var?
                        (p/assignment-exp-node (resolve-exp left mp)
                                               (resolve-exp right mp)
                                               op)
                        (throw (ex-info "Invalid lvalue." {:lvalue e}))))
    :binary-exp (p/binary-exp-node (resolve-exp (:left e) mp)
                                   (resolve-exp (:right e) mp)
                                   (:binary-operator e))
    :unary-exp (p/unary-exp-node (:unary-operator e) (resolve-exp (:value e) mp))
    :conditional-exp (p/conditional-exp-node (resolve-exp (:left e) mp)
                                             (resolve-exp (:middle e) mp)
                                             (resolve-exp (:right e) mp))
    (throw (ex-info "Analyzer error. Invalid expression type" {:exp e}))))

(defn- resolve-declaration [d mp]
  (if (and (contains? mp (:identifier d)) (:from-current-block (get mp (:identifier d))))
    (throw (ex-info "Analyzer error. Duplicate variable declaration." {:declaration d}))
    (let [ident (:identifier d)
          unique-name (unique-identifier ident)
          updated-mp (assoc mp ident {:name unique-name
                                      :from-current-block true})
          init (when (:initial d) (resolve-exp (:initial d) updated-mp))]
      (if init
        {:declaration (p/declaration-node unique-name init)
         :variable-map updated-mp}
        {:declaration (p/declaration-node unique-name)
         :variable-map updated-mp}))))

(defn- resolve-statement [s mp]
  (condp = (:statement-type s)
    :return (p/return-statement-node (resolve-exp (:value s) mp))
    :expression (p/expression-statement-node (resolve-exp (:value s) mp))
    :if (if (:else-statement s)
          (p/if-statement-node (resolve-exp (:condition s) mp)
                               (resolve-statement (:then-statement s) mp)
                               (resolve-statement (:else-statement s) mp))
          (p/if-statement-node (resolve-exp (:condition s) mp)
                               (resolve-statement (:then-statement s) mp)))
    :compound (let [updated-mp (zipmap (keys mp)
                                       (map (fn [m]
                                              (update m :from-current-block (fn [_] false)))
                                            (vals mp)))]
                (p/compound-statement-node (:block (resolve-block (:block s) updated-mp))))
    :empty (p/empty-statement-node)
    (throw (ex-info "Analyzer error. Invalid statement." {:statement s}))))

(defn- resolve-block-item [item mp]
  (let [type (:type item)]
    (cond
      (= type :declaration) (let [v (resolve-declaration item mp)]
                              {:block-item (:declaration v)
                               :variable-map (:variable-map v)})
      (= type :statement) {:block-item (resolve-statement item mp)
                           :variable-map mp}
      :else (throw (ex-info "Analyzer Error. Invalid statement/declaration." {item item})))))

(defn- resolve-block
  ([block]
   (resolve-block block {}))
  ([block var-mp]
   (reduce (fn [acc block-item]
             (let [v (resolve-block-item block-item (:variable-map acc))]
               {:block (conj (:block acc) (:block-item v))
                :variable-map (:variable-map v)}))
           {:block []
            :variable-map var-mp}
           block)))

(defn- validate-function [f]
  (let [updated-body (resolve-block (:body f))]
    (assoc f :body (:block updated-body))))

(comment

  (resolve-block
   [{:type :declaration
     :identifier "a",
     :initial {:type :exp, :exp-type :constant-exp, :value 1}}
    {:type :statement,
     :statement-type :return,
     :value {:type :exp, :exp-type :constant-exp, :value 0}}])

  ())

(defn validate [ast]
  (map validate-function ast))

(defn- validate-from-src [s]
  (u/reset-counter!)
  (-> s
      l/lex
      p/parse
      validate))

(comment

  (pp/pprint
   (validate-from-src
    "int main (void) {
int a = 3;
{
  int a = a = 4;
}
return a;
}"))

  (pp/pprint
   (validate-from-src
    "int main (void) {
int x = 1 + x;
}"))

  ())
