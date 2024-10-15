(ns cljcc.analyzer
  (:require [cljcc.lexer :as l]
            [clojure.pprint :as pp]
            [cljcc.util :as u]
            [cljcc.parser :as p]))

(defn- unique-identifier
  ([] (unique-identifier "analyzer"))
  ([identifier] (u/create-identifier! identifier)))

(defn- copy-identifier-map
  "Returns a copy of the identifier map.

  Sets :from-current-block as false for every entry. Used when going into a inner scope."
  [identifier-map]
  (zipmap (keys identifier-map)
          (map (fn [m]
                 (update m :from-current-block (fn [_] false)))
               (vals identifier-map))))

(declare resolve-block)

(defn- resolve-exp [e identifier-map]
  (condp = (:exp-type e)
    :constant-exp e
    :variable-exp (if (contains? identifier-map (:identifier e))
                    (p/variable-exp-node (:name (get identifier-map (:identifier e))))
                    (throw (ex-info "Undeclared variable seen." {:variable e})))
    :assignment-exp (let [left (:left e)
                          right (:right e)
                          op (:assignment-operator e)
                          left-var? (= :variable-exp (:exp-type left))]
                      (if left-var?
                        (p/assignment-exp-node (resolve-exp left identifier-map)
                                               (resolve-exp right identifier-map)
                                               op)
                        (throw (ex-info "Invalid lvalue." {:lvalue e}))))
    :binary-exp (p/binary-exp-node (resolve-exp (:left e) identifier-map)
                                   (resolve-exp (:right e) identifier-map)
                                   (:binary-operator e))
    :unary-exp (p/unary-exp-node (:unary-operator e) (resolve-exp (:value e) identifier-map))
    :conditional-exp (p/conditional-exp-node (resolve-exp (:left e) identifier-map)
                                             (resolve-exp (:middle e) identifier-map)
                                             (resolve-exp (:right e) identifier-map))
    :function-call-exp (let [fn-name (:identifier e)
                             args (:arguments e)]
                         (if (contains? identifier-map fn-name)
                           (p/function-call-exp-node (:new-name (get identifier-map fn-name))
                                                     (map #(resolve-exp % identifier-map) args))
                           (throw (ex-info "Undeclared function !" {:function-name fn-name}))))
    (throw (ex-info "Analyzer error. Invalid expression type" {:exp e}))))

(defn- resolve-optional-exp [e identifier-map]
  (if (nil? e)
    e
    (resolve-exp e identifier-map)))

(defn- resolve-variable-declaration
  "Resolves variable declarations.

  Ensures variable not declared twice in the current scope."
  [{:keys [identifier initial] :as d} identifier-map]
  (if (and (contains? identifier-map identifier)
           (:from-current-block (get identifier-map identifier)))
    (throw (ex-info "Analyzer error. Duplicate variable declaration." {:declaration d}))
    (let [unique-name (unique-identifier identifier)
          updated-identifier-map (assoc identifier-map identifier {:name unique-name
                                                                   :from-current-block true
                                                                   :has-linkage false})
          init-value (when initial (resolve-exp initial updated-identifier-map))]
      {:declaration (p/variable-declaration-node unique-name init-value)
       :identifier-map updated-identifier-map})))

(defn- resolve-parameter [{:keys [identifier] :as param} identifier-map]
  (if (and (contains? identifier-map identifier)
           (:from-current-block (get identifier-map identifier)))
    (throw (ex-info "Analyzer error. Parameter name duplicated." {:parameter param}))
    (let [unique-name (unique-identifier identifier)
          updated-identifier-map (assoc identifier-map identifier {:name unique-name
                                                                   :from-current-block true
                                                                   :has-linkage false})]
      {:parameter (p/variable-declaration-node unique-name)
       :identifier-map updated-identifier-map})))

(defn- resolve-parameters [params identifier-map]
  (reduce (fn [acc p]
            (let [{:keys [parameter identifier-map]} (resolve-parameter p (:identifier-map acc))]
              {:parameters (conj (:parameters acc) parameter)
               :identifier-map identifier-map}))
          {:parameters [] :identifier-map identifier-map}
          params))

(defn- resolve-function-declaration
  "Resolve function declaration.

  Ensures functions not declared twice in current scope with incorrect linkage."
  [{:keys [identifier parameters return-type body] :as d} identifier-map]
  (let [prev-entry (get identifier-map identifier)
        illegally-redeclared? (and (contains? identifier-map identifier)
                                   (:from-current-scope prev-entry)
                                   (not (:has-linkage prev-entry)))
        _ (when illegally-redeclared?
            (throw (ex-info "Analzer Error. Function duplicate declaration." {:declaration d})))
        updated-identifier-map (assoc identifier-map identifier {:new-name identifier
                                                                 :name identifier
                                                                 :from-current-scope true
                                                                 :has-linkage true})
        inner-map (copy-identifier-map updated-identifier-map)
        {new-params :parameters, inner-map :identifier-map} (resolve-parameters parameters inner-map)
        new-body (when body (resolve-block body inner-map))]
    {:declaration (p/function-declaration-node return-type identifier new-params (:block new-body))
     :identifier-map updated-identifier-map}))

(defn- resolve-declaration [{:keys [declaration-type] :as d} identifier-map]
  (condp = declaration-type
    :variable (resolve-variable-declaration d identifier-map)
    :function (resolve-function-declaration d identifier-map)
    (throw (ex-info "Analyzer Error. Invalid declaration type." {:declaration d}))))

(defn- resolve-for-init [for-init var-mp]
  (if (= (:type for-init) :declaration)
    (resolve-declaration for-init var-mp)
    (resolve-optional-exp for-init var-mp)))

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
    :break s
    :continue s
    :while (p/while-statement-node (resolve-exp (:condition s) mp)
                                   (resolve-statement (:body s) mp))
    :do-while (p/do-while-statement-node (resolve-exp (:condition s) mp)
                                         (resolve-statement (:body s) mp))
    :for (let [new-identifier-map (copy-identifier-map mp)
               for-init (resolve-for-init (:init s) new-identifier-map)
               new-var-map (if (:declaration for-init)
                             (:identifier-map for-init)
                             new-identifier-map) ; updates new-identifier-map so that include possible
                                          ; variable declaration
               for-init (if (:declaration for-init)
                          (:declaration for-init)
                          for-init)
               condition (resolve-optional-exp (:condition s) new-var-map)
               post (resolve-optional-exp (:post s) new-var-map)
               body (resolve-statement (:body s) new-var-map)]
           (p/for-statement-node for-init condition post body))
    :compound (let [updated-mp (copy-identifier-map mp)]
                (p/compound-statement-node (:block (resolve-block (:block s) updated-mp))))
    :empty (p/empty-statement-node)
    (throw (ex-info "Analyzer error. Invalid statement." {:statement s}))))

(defn- resolve-block-item [item identifier-map]
  (let [type (:type item)]
    (cond
      (= type :declaration) (let [v (resolve-declaration item identifier-map)]
                              {:block-item (:declaration v)
                               :identifier-map (:identifier-map v)})
      (= type :statement) {:block-item (resolve-statement item identifier-map)
                           :identifier-map identifier-map}
      :else (throw (ex-info "Analyzer Error. Invalid statement/declaration." {item item})))))

(defn- resolve-block
  ([block]
   (resolve-block block {}))
  ([block identifier-map]
   (reduce (fn [acc block-item]
             (let [v (resolve-block-item block-item (:identifier-map acc))]
               {:block (conj (:block acc) (:block-item v))
                :identifier-map (:identifier-map v)}))
           {:block []
            :identifier-map identifier-map}
           block)))

(defn- annotate-label [m l]
  (assoc m :label l))

(defn- label-statement
  ([s]
   (label-statement s nil))
  ([s current-label]
   (let [s-type (:statement-type s)]
     (cond
       (= s-type :break) (if (nil? current-label)
                           (throw (ex-info "break statement outside of loop" {}))
                           (p/break-statement-node current-label))
       (= s-type :continue) (if (nil? current-label)
                              (throw (ex-info "continue statement outside of loop" {}))
                              (p/continue-statement-node current-label))
       (= s-type :while) (let [new-label (unique-identifier "while_label")
                               l-body (label-statement (:body s) new-label)
                               l-while  (p/while-statement-node (:condition s) l-body)]
                           (annotate-label l-while new-label))
       (= s-type :do-while) (let [new-label (unique-identifier "do_while_label")
                                  l-body (label-statement (:body s) new-label)
                                  l-do-while (p/do-while-statement-node (:condition s) l-body)]
                              (annotate-label l-do-while new-label))
       (= s-type :for) (let [new-label (unique-identifier "for_label")
                             l-body (label-statement (:body s) new-label)
                             l-for (p/for-statement-node (:init s) (:condition s) (:post s) l-body)]
                         (annotate-label l-for new-label))
       (= s-type :if) (if (:else-statement s)
                        (p/if-statement-node (:condition s)
                                             (label-statement (:then-statement s) current-label)
                                             (label-statement (:else-statement s) current-label))
                        (p/if-statement-node (:condition s)
                                             (label-statement (:then-statement s) current-label)))
       (= s-type :compound) (let [update-block-f (fn [item]
                                                   (if (= (:type item) :statement)
                                                     (label-statement item current-label)
                                                     item))
                                  new-block (map update-block-f (:block s))]
                              (p/compound-statement-node new-block))
       (= s-type :return) s
       (= s-type :expression) s
       (= s-type :empty) s
       :else (throw (ex-info "invalid statement reached during loop labelling." {}))))))

(defn- resolve-loop-label [body]
  (let [f (fn [item]
            (if (= :statement (:type item))
              (label-statement item)
              item))
        new-body (map f body)]
    new-body))

(defn- validate-loop-labels [{block :block}]
  (map (fn [b]
         (assoc b :body (resolve-loop-label (:body b)))) block))

(defn validate [ast]
  (-> ast
      resolve-block
      validate-loop-labels))

(defn- validate-from-src [s]
  (u/reset-counter!)
  (-> s
      l/lex
      p/parse
      validate))

(comment

  (pp/pprint
   (validate-from-src
    "
int main(void) {
int foo(void);
return foo;
}

int foo(void) {
return 1;
}
"))

  ())
