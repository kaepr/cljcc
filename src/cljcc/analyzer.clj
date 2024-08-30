(ns cljcc.analyzer
  (:require [cljcc.lexer :as l]
            [clojure.pprint :as pp]
            [cljcc.util :as u]
            [cljcc.parser :as p]))

(defn- unique-identifier
  ([] (unique-identifier "analyzer"))
  ([identifier] (u/create-identifier! identifier)))

(defn- resolve-exp [e mp]
  (condp = (:exp-type e)
    :constant-exp e
    :variable-exp (if (contains? mp (:identifier e))
                    (p/variable-exp-node (get mp (:identifier e)))
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
  (if (contains? mp (:identifier d))
    (throw (ex-info "Analyzer error. Duplicate variable declaration." {:declaration d}))
    (let [ident (:identifier d)
          unique-name (unique-identifier ident)
          updated-mp (assoc mp ident unique-name)
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
    :empty (p/empty-statement-node)
    (throw (ex-info "Analyzer error. Invalid statement." {:statement s}))))

(defn- resolve-block-item [item mp]
  (let [type (:type item)]
    (cond
      (= type :declaration) (let [v (resolve-declaration item mp)]
                              {:item (:declaration v)
                               :variable-map (:variable-map v)})
      (= type :statement) {:item (resolve-statement item mp)
                           :variable-map mp}
      :else (throw (ex-info "Analyzer Error. Invalid statement/declaration." {item item})))))

(defn- validate-function [f]
  (let [updated-body (reduce
                      (fn [acc item]
                        (let [v (resolve-block-item item (:variable-map acc))]
                          {:body (conj (:body acc) (:item v))
                           :variable-map (:variable-map v)}))
                      {:body []
                       :variable-map {}}
                      (:body f))]
    (assoc f :body (:body updated-body))))

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
;
return 0;
}"))

  (pp/pprint
   (validate-from-src
    "int main (void) {
int x;
int a = -1;
int b = 2;

int c = b = 4 + 4;
return 12 / 12321312 + 12312 % 4;
}"))

  ())
