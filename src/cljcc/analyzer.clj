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
        already-declared-var? (and (contains?  identifier-map identifier)
                                   (:from-current-block (get identifier-map identifier))
                                   (not (:has-linkage prev-entry)))
        illegally-redeclared? (and (contains? identifier-map identifier)
                                   (:from-current-scope prev-entry)
                                   (not (:has-linkage prev-entry)))
        inside-function-definition? (:inside-function-definition (:inside-inner-scope identifier-map))
        _ (when already-declared-var?
            (throw (ex-info "Analyzer Error. Variable already declared in same scope." {:declaration d})))
        _ (when illegally-redeclared?
            (throw (ex-info "Analyzer Error. Function duplicate declaration." {:declaration d})))
        updated-identifier-map (assoc identifier-map identifier {:new-name identifier
                                                                 :name identifier
                                                                 :from-current-block true
                                                                 :from-current-scope true
                                                                 :has-linkage true})
        inner-map (copy-identifier-map updated-identifier-map)
        {new-params :parameters, inner-map :identifier-map} (resolve-parameters parameters inner-map)
        _ (when (and body inside-function-definition?)
            (throw (ex-info "Analyzer Error. Nested function definition not allowed." {:declaration d})))
        new-body (when body (resolve-block body (assoc inner-map :inside-inner-scope {:inside-function-definition true})))]
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

(defn- resolve-block-item [{:keys [type] :as item} identifier-map]
  (condp = type
    :declaration (let [v (resolve-declaration item identifier-map)]
                   {:block-item (:declaration v)
                    :identifier-map (:identifier-map v)})
    :statement {:block-item (resolve-statement item identifier-map)
                :identifier-map identifier-map}
    (throw (ex-info "Analyzer Error. Invalid statement/declaration." {item item}))))

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

(defn- typecheck-exp
  "Returns the expression itself, after typechecking all subexpressions."
  [{:keys [exp-type] :as e} decl-name->symbol]
  (condp = exp-type
    :constant-exp e
    :variable-exp (let [identifier (:identifier e)
                        variable-type (:variable-type (get decl-name->symbol identifier))
                        _ (when  (not (= :int variable-type))
                            (throw (ex-info "Analyzer Error. Function name used as variable." {:exp e})))]
                    e)
    :assignment-exp (do
                      (typecheck-exp (:left e) decl-name->symbol)
                      (typecheck-exp (:right e) decl-name->symbol)
                      e)
    :binary-exp (do
                  (typecheck-exp (:left e) decl-name->symbol)
                  (typecheck-exp (:right e) decl-name->symbol)
                  e)
    :unary-exp (do
                 (typecheck-exp (:value e) decl-name->symbol)
                 e)
    :conditional-exp (do
                       (typecheck-exp (:left e) decl-name->symbol)
                       (typecheck-exp (:right e) decl-name->symbol)
                       (typecheck-exp (:middle e) decl-name->symbol)
                       e)
    :function-call-exp (let [symbol (decl-name->symbol (:identifier e))
                             _ (when (= :int (:variable-type symbol))
                                 (throw (ex-info "Analyzer Error. Variable used as function name." {:exp e})))
                             _ (when (not (= (count (:arguments e)) (:param-count symbol)))
                                 (throw (ex-info "Analyzer Error. Function called with the wrong number of arguments." {:exp e})))
                             _ (map #(typecheck-exp % decl-name->symbol) (:arguments e))]
                         e)
    (throw (ex-info "Analyzer error. Invalid expression type passed to typechecker." {:exp e}))))

(defn- variable-symbol [variable-type]
  {:variable-type variable-type})

(defn- function-symbol [param-count defined?]
  {:param-count param-count
   :defined? defined?})

(defn- add-parameters [params decl-name->symbol]
  (if (= 0 (count params))
    decl-name->symbol
    (apply assoc
           decl-name->symbol
             (flatten (map (fn [p] [(:identifier p) (variable-symbol :int)]) params)))))

(declare typecheck-block)

(defn- typecheck-declaration [{:keys [declaration-type identifier] :as d} decl-name->symbol]
  (condp = declaration-type
    :variable (let [updated-decl-name->symbol (assoc decl-name->symbol identifier (variable-symbol :int))
                    _ (when (:initial d) (typecheck-exp (:initial d) updated-decl-name->symbol))]
                {:declaration d
                 :decl-name->symbol updated-decl-name->symbol})
    :function (let [param-count (count (:parameters d))
                    has-body? (not (empty? (:body d)))
                    previously-declared? (contains? decl-name->symbol identifier)
                    _ (when previously-declared?
                        (let [old-symbol (get decl-name->symbol identifier)
                              _ (when (not= param-count (:param-count old-symbol))
                                  (throw (ex-info "Analyzer Error. Incompatible function declarations." {:declaration d})))
                              _ (when (and (:defined? old-symbol) has-body?)
                                  (throw (ex-info "Analyzer Error. Function is defined more than once." {:declaration d})))]))
                    updated-decl-name->symbol (assoc decl-name->symbol
                                                     identifier
                                                     (function-symbol param-count (or (:defined? (get decl-name->symbol identifier)) has-body?)))]
                (if has-body?
                  (let [with-parameters-symbols (add-parameters (:parameters d) updated-decl-name->symbol)
                        with-body-symbols (typecheck-block (:body d) with-parameters-symbols)]
                   {:declaration d
                    :decl-name->symbol (:decl-name->symbol with-body-symbols)})
                  {:declaration d
                   :decl-name->symbol updated-decl-name->symbol}))
    (throw (ex-info "Analyzer Error. Invalid declaration for typechecker." {:declaration d}))))

(defn- typecheck-optional-expression [e decl-name->symbol]
  (if (nil? e)
    e
    (typecheck-exp e decl-name->symbol)))


(defn- typecheck-for-init [for-init decl-name->symbol]
  (if (= (:type for-init) :declaration)
    (typecheck-declaration for-init decl-name->symbol)
    (typecheck-optional-expression for-init decl-name->symbol)))

(defn- typecheck-statement [{:keys [statement-type] :as s} decl-name->symbol]
  (condp = statement-type
    :return (do
              (typecheck-exp (:value s) decl-name->symbol)
              s)
    :expression (do
                  (typecheck-exp (:value s) decl-name->symbol)
                  s)
    :if (if (:else-statement s)
          (do
            (typecheck-exp (:condition s) decl-name->symbol)
            (typecheck-statement (:then-statement s) decl-name->symbol)
            (typecheck-statement (:else-statement s) decl-name->symbol)
            s)
          (do
            (typecheck-exp (:condition s) decl-name->symbol)
            (typecheck-statement (:then-statement s) decl-name->symbol)
            s))
    :break s
    :continue s
    :while (do
             (typecheck-exp (:condition s) decl-name->symbol)
             (typecheck-statement (:body s) decl-name->symbol)
             s)
    :do-while (do
                (typecheck-exp (:condition s) decl-name->symbol)
                (typecheck-statement (:body s) decl-name->symbol)
                s)
    :for (let [f-init (typecheck-for-init (:init s) decl-name->symbol)
               updated-symbols (if (:declaration f-init)
                                 (:decl-name->symbol f-init)
                                 decl-name->symbol)
               _ (typecheck-optional-expression (:condition s) updated-symbols)
               _ (typecheck-optional-expression (:post s) updated-symbols)
               _ (typecheck-statement (:body s) updated-symbols)]
           s)
    :compound (do
                (typecheck-block (:block s) decl-name->symbol)
                s)
    :empty s
    (throw (ex-info "Analyzer Error. Invalid statement type in typechecker." {:statement s}))))

(defn- typecheck-item [{:keys [type] :as item} decl-name->symbol]
  (condp = type
    :declaration (let [v (typecheck-declaration item decl-name->symbol)]
                   {:block-item (:declaration v)
                    :decl-name->symbol (:decl-name->symbol v)})
    :statement {:block-item (typecheck-statement item decl-name->symbol)
                :decl-name->symbol decl-name->symbol}
    (throw (ex-info "Analyzer Error. Invalid statement/declaration." {item item}))))

(defn- typecheck-block
  ([block]
   (typecheck-block block {}))
  ([block decl-name->symbol]
   (reduce (fn [acc item]
             (let [v (typecheck-item item (:decl-name->symbol acc))]
               {:block (conj (:block acc) (:block-item v))
                :decl-name->symbol (:decl-name->symbol v)}))
           {:block []
            :decl-name->symbol decl-name->symbol}
           block)))

(defn validate [ast]
  (-> ast
      resolve-block
      validate-loop-labels
      typecheck-block))

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
int foo(void);
int main(void) {
return foo();
}
int foo(void) {
return 3;
}
"))

  ())
