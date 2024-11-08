(ns cljcc.analyzer
  (:require [cljcc.lexer :as l]
            [cljcc.util :as u]
            [cljcc.parser :as p]
            [cljcc.exception :as exc]))

(defn- unique-identifier
  ([] (unique-identifier "analyzer"))
  ([identifier] (u/create-identifier! identifier)))

(defn- copy-identifier-map
  "Returns a copy of the identifier map.

  Sets :at-top-level false, as it's going inside a scope. ( Could be fn definition, compound statement ).
  Sets :from-current-scope as false for every symbol. Used when going into a inner scope."
  [ident->symbol]
  (let [set-from-current-scope-as-false (fn [i->s]
                                          (zipmap (keys i->s)
                                                  (map (fn [s]
                                                         (assoc s :from-current-scope false))
                                                       (vals i->s))))]
    (-> ident->symbol
        (dissoc :at-top-level)
        set-from-current-scope-as-false
        (assoc :at-top-level false))))

(declare resolve-block)

(defn- resolve-exp [e ident->symbol]
  (condp = (:exp-type e)
    :constant-exp e
    :variable-exp (if (contains? ident->symbol (:identifier e))
                    (p/variable-exp-node (:name (get ident->symbol (:identifier e))))
                    (exc/analyzer-error "Undeclared variable seen." {:variable e}))
    :assignment-exp (let [left (:left e)
                          right (:right e)
                          op (:assignment-operator e)
                          left-var? (= :variable-exp (:exp-type left))]
                      (if left-var?
                        (p/assignment-exp-node (resolve-exp left ident->symbol)
                                               (resolve-exp right ident->symbol)
                                               op)
                        (exc/analyzer-error "Invalid lvalue in assignment expression." {:lvalue e})))
    :binary-exp (p/binary-exp-node (resolve-exp (:left e) ident->symbol)
                                   (resolve-exp (:right e) ident->symbol)
                                   (:binary-operator e))
    :unary-exp (p/unary-exp-node (:unary-operator e) (resolve-exp (:value e) ident->symbol))
    :conditional-exp (p/conditional-exp-node (resolve-exp (:left e) ident->symbol)
                                             (resolve-exp (:middle e) ident->symbol)
                                             (resolve-exp (:right e) ident->symbol))
    :function-call-exp (let [fn-name (:identifier e)
                             args (:arguments e)]
                         (if (contains? ident->symbol fn-name)
                           (p/function-call-exp-node (:new-name (get ident->symbol fn-name))
                                                     (map #(resolve-exp % ident->symbol) args))
                           (throw (ex-info "Undeclared function !" {:function-name fn-name}))))
    (exc/analyzer-error "Invalid expression." {:exp e})))

(defn- resolve-optional-exp [e identifier-map]
  (if (nil? e)
    e
    (resolve-exp e identifier-map)))

(defn- resolve-file-scope-variable-declaration
  "Adds file scope variable declaration to scope.

  Directly adds variable declaration to map as it is top level."
  [{:keys [identifier] :as declaration} ident->symbol]
  {:declaration declaration
   :ident->symbol (assoc ident->symbol identifier {:new-name identifier
                                                   :name identifier
                                                   :from-current-scope true
                                                   :has-linkage true})})

(defn- resolve-local-variable-declaration
  "Add local variable declaration.

  Validates for variables declared with same name.
  Validates for variables declared from different scope, but with conflicting storage class."
  [{:keys [identifier initial storage-class] :as declaration} ident->symbol]
  (let [prev-entry (get ident->symbol identifier)
        extern? (= storage-class :extern)
        _ (when (and prev-entry (:from-current-scope prev-entry))
            (when (not (and (:has-linkage prev-entry) extern?))
              (exc/analyzer-error "Conflicting local declaration." {:declaration declaration})))]
    (if extern?
      {:declaration declaration
       :ident->symbol (assoc ident->symbol identifier {:new-name identifier
                                                       :name identifier
                                                       :from-current-scope true
                                                       :has-linkage true})}
      (let [unique-name (unique-identifier identifier)
            updated-symbols (assoc ident->symbol identifier {:new-name unique-name
                                                             :name unique-name
                                                             :from-current-scope true
                                                             :has-linkage false})
            init-value (when initial (resolve-exp initial updated-symbols))]
        {:declaration (p/variable-declaration-node unique-name storage-class init-value)
         :ident->symbol updated-symbols}))))

(defn- resolve-variable-declaration
  "Resolves variable declarations.

  Ensures variable not declared twice in the current scope."
  [decl {:keys [at-top-level] :as ident->symbol}]
  (if at-top-level
    (resolve-file-scope-variable-declaration decl ident->symbol)
    (resolve-local-variable-declaration decl ident->symbol)))

(defn- resolve-parameter [{:keys [identifier] :as param} ident->symbol]
  (if (and (contains? ident->symbol identifier)
           (:from-current-scope (get ident->symbol identifier)))
    (exc/analyzer-error "Parameter name duplicated." {:parameter param})
    (let [unique-name (unique-identifier identifier)
          storage-class nil
          updated-identifier-map (assoc ident->symbol identifier {:name unique-name
                                                                  :from-current-scope true
                                                                  :has-linkage false})]
      {:parameter (p/variable-declaration-node unique-name storage-class)
       :ident->symbol updated-identifier-map})))

(defn- resolve-parameters [params ident->symbol]
  (reduce (fn [acc p]
            (let [{:keys [parameter ident->symbol]} (resolve-parameter p (:ident->symbol acc))]
              {:parameters (conj (:parameters acc) parameter)
               :ident->symbol ident->symbol}))
          {:parameters [] :ident->symbol ident->symbol}
          params))

(defn- resolve-function-declaration
  "Resolve function declaration.

  Ensures functions not declared twice in current scope with incorrect linkage."
  [{:keys [identifier storage-class parameters return-type body] :as d} ident->symbol]
  (let [prev-entry (get ident->symbol identifier)
        already-declared-var? (and (contains?  ident->symbol identifier)
                                   (:from-current-scope (get ident->symbol identifier))
                                   (not (:has-linkage prev-entry)))
        illegally-redeclared? (and (contains? ident->symbol identifier)
                                   (:from-current-scope prev-entry)
                                   (not (:has-linkage prev-entry)))
        static? (= :static storage-class)
        inside-function-definition? (not (:at-top-level ident->symbol))
        _ (when already-declared-var?
            (exc/analyzer-error "Variable already declared in same scope." {:declaration d}))
        _ (when illegally-redeclared?
            (exc/analyzer-error "Function duplicate declaration." {:declaration d}))
        updated-identifier-map (assoc ident->symbol identifier {:new-name identifier
                                                                :name identifier
                                                                :from-current-scope true
                                                                :has-linkage true})
        inner-map (copy-identifier-map updated-identifier-map)
        {new-params :parameters, inner-map :ident->symbol} (resolve-parameters parameters inner-map)
        _ (when (and body inside-function-definition?)
            (exc/analyzer-error "Nested function definition not allowed." {:declaration d
                                                                           :ident->symbol ident->symbol}))
        _ (when (and inside-function-definition? static?)
            (exc/analyzer-error "Nested static function declarations cannot exist." {:declaration d}))
        new-body (when body (resolve-block body inner-map))]
    {:declaration (p/function-declaration-node return-type storage-class identifier new-params (:block new-body))
     :ident->symbol updated-identifier-map}))

(defn- resolve-declaration [{:keys [declaration-type] :as d} ident->symbol]
  (condp = declaration-type
    :variable (resolve-variable-declaration d ident->symbol)
    :function (resolve-function-declaration d ident->symbol)
    (throw (ex-info "Analyzer Error. Invalid declaration type." {:declaration d}))))

(defn- resolve-for-init [for-init ident->symbol]
  (if (= (:type for-init) :declaration)
    (resolve-declaration for-init ident->symbol)
    (resolve-optional-exp for-init ident->symbol)))

(defmulti resolve-statement
  "Resolves statements in a given scope.

  Scope here refers to the ident->symbol map, which holds declarations
  visisble to statement at this time.

  Dispatches based on the type of statement.

  Returns statement after recursively resolving all expressions and statements.
  "
  (fn [statement _ident->symbol]
    (:statement-type statement)))

(defmethod resolve-statement :default [statement _]
  (exc/analyzer-error "Invalid statement." {:statement statement}))

(defmethod resolve-statement :return [{:keys [value]} ident->symbol]
  (p/return-statement-node (resolve-exp value ident->symbol)))

(defmethod resolve-statement :break [statement _]
  statement)

(defmethod resolve-statement :continue [statement _]
  statement)

(defmethod resolve-statement :empty [statement _]
  statement)

(defmethod resolve-statement :expression [{:keys [value]} ident->symbol]
  (p/expression-statement-node (resolve-exp value ident->symbol)))

(defmethod resolve-statement :if [{:keys [condition then-statement else-statement]} ident->symbol]
  (if else-statement
    (p/if-statement-node (resolve-exp condition ident->symbol)
                         (resolve-statement then-statement ident->symbol)
                         (resolve-statement else-statement ident->symbol))
    (p/if-statement-node (resolve-exp condition ident->symbol)
                         (resolve-statement then-statement ident->symbol))))

(defmethod resolve-statement :while [{:keys [condition body]} ident->symbol]
  (p/while-statement-node (resolve-exp condition ident->symbol)
                          (resolve-statement body ident->symbol)))

(defmethod resolve-statement :do-while [{:keys [condition body]} ident->symbol]
  (p/do-while-statement-node (resolve-exp condition ident->symbol)
                             (resolve-statement body ident->symbol)))

(defmethod resolve-statement :for [{:keys [init condition post body]} ident->symbol]
  (let [for-scope-identifier-map (copy-identifier-map ident->symbol)
        resolved-for-init (resolve-for-init init for-scope-identifier-map)
        for-scope-identifier-map (if (:declaration resolved-for-init) ; updates symbol map if for initializer is declaration
                                   (:ident->symbol resolved-for-init)
                                   for-scope-identifier-map)
        resolved-for-init (if (:declaration resolved-for-init) ; getting the underlying declaration, if it is
                            (:declaration resolved-for-init)
                            resolved-for-init)
        condition (resolve-optional-exp condition for-scope-identifier-map)
        post (resolve-optional-exp post for-scope-identifier-map)
        body (resolve-statement body for-scope-identifier-map)]
    (p/for-statement-node resolved-for-init condition post body)))

(defmethod resolve-statement :compound [{:keys [block]} ident->symbol]
  (p/compound-statement-node (:block (resolve-block block (copy-identifier-map ident->symbol)))))

(defn- resolve-block-item [{:keys [type] :as item} ident->symbol]
  (condp = type
    :declaration (let [v (resolve-declaration item ident->symbol)]
                   {:block-item (:declaration v)
                    :ident->symbol (:ident->symbol v)})
    :statement {:block-item (resolve-statement item ident->symbol)
                :ident->symbol ident->symbol}
    (exc/analyzer-error "Invalid statement/declaration type." item)))

(defn- resolve-block
  "Resolves a block with a given symbol table.

  ident->symbol holds identifier to symbol mapping.
  Symbol contains the type information, generated variable name etc.

  | key            | description |
  |----------------|-------------|
  |`:at-top-level` | Is current level top or not ( default true)|"
  ([block]
   (resolve-block block {:at-top-level true}))
  ([block ident->symbol]
   (reduce (fn [acc block-item]
             (let [v (resolve-block-item block-item (:ident->symbol acc))]
               {:block (conj (:block acc) (:block-item v))
                :ident->symbol (:ident->symbol v)}))
           {:block []
            :ident->symbol ident->symbol}
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
  [{:keys [exp-type] :as e} ident->symbol]
  (condp = exp-type
    :constant-exp e
    :variable-exp (let [identifier (:identifier e)
                        var? (= :variable (:type (get ident->symbol identifier)))
                        _ (when (not var?)
                            (exc/analyzer-error "Function name used as variable." {:exp e :ident->symbol ident->symbol}))]
                    e)
    :assignment-exp (do
                      (typecheck-exp (:left e) ident->symbol)
                      (typecheck-exp (:right e) ident->symbol)
                      e)
    :binary-exp (do
                  (typecheck-exp (:left e) ident->symbol)
                  (typecheck-exp (:right e) ident->symbol)
                  e)
    :unary-exp (do
                 (typecheck-exp (:value e) ident->symbol)
                 e)
    :conditional-exp (do
                       (typecheck-exp (:left e) ident->symbol)
                       (typecheck-exp (:right e) ident->symbol)
                       (typecheck-exp (:middle e) ident->symbol)
                       e)
    :function-call-exp (let [symbol (ident->symbol (:identifier e))
                             _ (when (not= :function (:type symbol))
                                 (throw (ex-info "Analyzer Error. Variable used as function name." {:exp e})))
                             _ (when (not= (count (:arguments e)) (:param-count symbol))
                                 (throw (ex-info "Analyzer Error. Function called with the wrong number of arguments." {:exp e})))
                             _ (map #(typecheck-exp % ident->symbol) (:arguments e))]
                         e)
    (throw (ex-info "Analyzer error. Invalid expression type passed to typechecker." {:exp e}))))

(defn- fun-attrs [defined? global?]
  {:type :fun
   :defined? defined?
   :global? global?})

(defn- static-attrs [initial-value global?]
  {:type :static
   :initial-value initial-value
   :global? global?})

(defn- local-attrs []
  {:type :local})

(defn- variable-symbol [variable-type attrs]
  {:type :variable
   :variable-type variable-type
   :attrs attrs})

(defn- function-symbol [param-count attrs]
  {:type :function
   :param-count param-count
   :attrs attrs})

(defn- add-parameters [params ident->symbol]
  (if (zero? (count params))
    ident->symbol
    (apply assoc
           ident->symbol
           (flatten (map (fn [p] [(:identifier p) (variable-symbol :int (local-attrs))]) params)))))

(declare typecheck-block)

(defn- validate-fn-decl-and-return-updated-attrs
  [cur-decl old-decl]
  (let [param-count (count (:parameters cur-decl))
        old-param-count (:param-count old-decl)
        has-body? (seq (:body cur-decl))
        _ (when (not= param-count old-param-count)
            (exc/analyzer-error "Incompatible function declarations." {:declaration1 old-decl
                                                                       :declaration2 cur-decl}))
        defined? (:defined? (:attrs old-decl))
        _ (when (and defined? has-body?)
            (exc/analyzer-error "Function is defined more than once." {:declaration cur-decl}))
        old-global? (:global? (:attrs old-decl))
        _ (when (and old-global? (= :static (:storage-class cur-decl)))
            (exc/analyzer-error "Static function definition follows non static." {:declaration cur-decl}))]
    {:defined? defined?
     :global? old-global?}))

(defn- typecheck-function-declaration
  [{:keys [identifier parameters body storage-class] :as decl} ident->symbol]
  (let [param-count (count parameters)
        body? (seq body)
        old-decl (get ident->symbol identifier)
        {defined? :defined?
         global? :global?} (if old-decl
                             (validate-fn-decl-and-return-updated-attrs decl old-decl)
                             {:defined? false
                              :global? (not= :static storage-class)})
        attrs (fun-attrs (or defined? (boolean body?)) global?)
        updated-symbol-map (assoc ident->symbol identifier
                                  (function-symbol param-count attrs))]
    (if body?
      (let [with-parameter-symbols (add-parameters parameters updated-symbol-map)
            with-body-symbols (typecheck-block body (assoc with-parameter-symbols
                                                           :at-top-level false))]
        {:declaration decl
         :ident->symbol (assoc (:ident->symbol with-body-symbols) :at-top-level true)})
      {:declaration decl
       :ident->symbol updated-symbol-map})))

(defn- get-initial-value [decl]
  (cond
    (= :constant-exp (:exp-type (:initial decl))) {:type :initial
                                                   :value (:value (:initial decl))}
    (nil? (:initial decl))    (if (= :extern (:storage-class decl))
                                {:type :no-initializer}
                                {:type :tentative})
    :else (exc/analyzer-error "Non-constant initializer!" decl)))

(defn- validate-file-scope-decl-return-attrs [cur-decl old-decl]
  (let [_ (when (not= :variable (:type old-decl))
            (exc/analyzer-error "Function redeclared as variable." {:declaration1 old-decl
                                                                    :declaration2 cur-decl}))
        global? (not= :static (:storage-class cur-decl))
        global? (cond
                  (= :extern (:storage-class cur-decl)) (:global? (:attrs old-decl))
                  (not= global? (:global? (:attrs old-decl))) (exc/analyzer-error "Conflicting variable linkage." {:declaration1 old-decl
                                                                                                                   :declaration2 cur-decl})
                  :else global?)
        initial-value (get-initial-value cur-decl)
        initial-value (cond
                        (=
                         :initial
                         (get-in old-decl [:attrs :initial-value :type])) (if (= (:type initial-value) :initial)
                                                                            (exc/analyzer-error "Conflivting file scope variable definition." {:declarartion1 old-decl
                                                                                                                                               :declaration2 cur-decl})
                                                                            (get-in old-decl [:attrs :initial-value]))
                        (and
                         (= :tentative (get-in old-decl [:attrs :initial-value :type]))
                         (not= :initial (:type initial-value))) {:type :tentative}
                        :else initial-value)]
    {:global? global?
     :initial-value initial-value}))

(defn- typecheck-file-scope-variable-declaration
  [{:keys [identifier storage-class] :as d} ident->symbol]
  (let [old-decl (get ident->symbol identifier)
        global? (not= :static storage-class)
        initial-value (get-initial-value d)
        {global? :global?
         initial-value :initial-value} (if old-decl
                                         (validate-file-scope-decl-return-attrs d old-decl)
                                         {:global? global?
                                          :initial-value initial-value})]
    {:declaration d
     :ident->symbol (assoc ident->symbol
                           identifier
                           (variable-symbol :int (static-attrs initial-value global?)))}))

(defn- typecheck-local-scope-variable-declaration
  [{:keys [identifier storage-class initial] :as d} ident->symbol]
  (cond
    (= :extern storage-class) (let [_ (when (not (nil? initial))
                                        (exc/analyzer-error "Initializer on local extern variable declaration." d))
                                    old-decl (get ident->symbol identifier)
                                    _ (when (and old-decl (not= :variable (:type old-decl)))
                                        (exc/analyzer-error "Function redeclared as variable." {:declaration1 old-decl
                                                                                                :declaration2 d}))
                                    updated-symbols (if old-decl
                                                      ident->symbol
                                                      (assoc ident->symbol
                                                             identifier
                                                             (variable-symbol :int (static-attrs {:type :no-initializer} true))))]
                                {:declaration d
                                 :ident->symbol updated-symbols})
    (= :static storage-class) (let [initial-value (cond
                                                    (= :constant-exp (:exp-type initial)) {:type :initial
                                                                                           :value (:value initial)}
                                                    (nil? initial) {:type :initial
                                                                    :value 0}
                                                    :else (exc/analyzer-error "Non-constant initializer on local static variable." d))]
                                {:declaration d
                                 :ident->symbol (assoc ident->symbol
                                                       identifier
                                                       (variable-symbol :int (static-attrs initial-value false)))})
    :else (let [updated-symbols (assoc ident->symbol identifier (variable-symbol :int (local-attrs)))
                _ (when initial (typecheck-exp initial updated-symbols))]
            {:declaration d
             :ident->symbol updated-symbols})))

(defn- typecheck-declaration [{:keys [declaration-type] :as d} ident->symbol]
  (let [at-top-level? (:at-top-level ident->symbol)]
    (condp = declaration-type
      :variable (if at-top-level?
                  (typecheck-file-scope-variable-declaration d ident->symbol)
                  (typecheck-local-scope-variable-declaration d ident->symbol))
      :function (typecheck-function-declaration d ident->symbol)
      (throw (ex-info "Analyzer Error. Invalid declaration for typechecker." {:declaration d})))))

(defn- typecheck-optional-expression [e ident->symbol]
  (if (nil? e)
    e
    (typecheck-exp e ident->symbol)))

(defn- typecheck-for-init [for-init ident->symbol]
  (if (= (:type for-init) :declaration)
    (typecheck-declaration for-init ident->symbol)
    (typecheck-optional-expression for-init ident->symbol)))

;; TODO: typechecking must thread through all recursive typecheck statement
(defn- typecheck-statement [{:keys [statement-type] :as s} ident->symbol]
  (condp = statement-type
    :return (do
              (typecheck-exp (:value s) ident->symbol)
              {:statement s
               :ident->symbol ident->symbol})
    :expression (do
                  (typecheck-exp (:value s) ident->symbol)
                  {:statement s
                   :ident->symbol ident->symbol})
    :if (if (:else-statement s)
          (do
            (typecheck-exp (:condition s) ident->symbol)
            (typecheck-statement (:then-statement s) ident->symbol)
            (typecheck-statement (:else-statement s) ident->symbol)
            {:statement s
             :ident->symbol ident->symbol})
          (do
            (typecheck-exp (:condition s) ident->symbol)
            (typecheck-statement (:then-statement s) ident->symbol)
            {:statement s
             :ident->symbol ident->symbol}))
    :break {:statement s
            :ident->symbol ident->symbol}
    :continue {:statement s
               :ident->symbol ident->symbol}
    :while (do
             (typecheck-exp (:condition s) ident->symbol)
             (typecheck-statement (:body s) ident->symbol)
             {:statement s
              :ident->symbol ident->symbol})
    :do-while (do
                (typecheck-exp (:condition s) ident->symbol)
                (typecheck-statement (:body s) ident->symbol)
                {:statement s
                 :ident->symbol ident->symbol})
    :for (let [f-init (typecheck-for-init (:init s) ident->symbol)
               updated-symbols (if (:declaration f-init)
                                 (:ident->symbol f-init)
                                 ident->symbol)
               _ (typecheck-optional-expression (:condition s) updated-symbols)
               _ (typecheck-optional-expression (:post s) updated-symbols)
               _ (typecheck-statement (:body s) updated-symbols)]
           {:statement s
            :ident->symbol ident->symbol})
    ;; TODO: Standardize returning map from statements
    :compound (let [v (typecheck-block (:block s) ident->symbol)]
                {:statement s
                 :ident->symbol (:ident->symbol v)})
    :empty {:statement s
            :ident->symbol ident->symbol}
    (throw (ex-info "Analyzer Error. Invalid statement type in typechecker." {:statement s}))))

(defn- typecheck-item [{:keys [type] :as item} ident->symbol]
  (condp = type
    :declaration (let [v (typecheck-declaration item ident->symbol)]
                   {:block-item (:declaration v)
                    :ident->symbol (:ident->symbol v)})
    :statement (let [v (typecheck-statement item ident->symbol)]
                 {:block-item (:statement v)
                  :ident->symbol (:ident->symbol v)})
    (exc/analyzer-error "Invalid statement/declaration." {item item})))

(defn- typecheck-block
  "Typechecks a block with a given symbol table.

  ident->symbol holds identifier to symbol mapping.
  Symbol contains the type information, generated variable name etc.

  | key            | description |
  |----------------|-------------|
  |`:at-top-level` | Is current level top or not ( default true)|"
  ([block]
   (typecheck-block block {:at-top-level true}))
  ([block ident->symbol]
   (reduce (fn [acc item]
             (let [v (typecheck-item item (:ident->symbol acc))]
               {:block (conj (:block acc) (:block-item v))
                :ident->symbol (:ident->symbol v)}))
           {:block []
            :ident->symbol ident->symbol}
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

  (validate-from-src
   "
int twice(int x){
    return 2 * x;
}
")

  (validate-from-src
   "
int main(void) {
return 2;
}
")

  (validate-from-src "
int main(void) {
    int x = 3;
    {
        extern int x;
    }
    return x;
}

static int x = 10;
")

  ())
