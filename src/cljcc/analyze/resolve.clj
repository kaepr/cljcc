(ns cljcc.analyze.resolve
  (:require [cljcc.exception :as exc]
            [cljcc.parser :as p]
            [malli.dev.pretty :as pretty]
            [cljcc.schema :as s]
            [cljcc.util :as util]
            [malli.core :as m]))

(defn- unique-identifier [identifier]
  (util/create-identifier! identifier))

(defn- copy-identifier-map
  "Returns a copy of the identifier -> symbol map.

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

(declare resolve-block resolve-declaration resolve-optional-exp)

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
    :cast-exp (p/cast-exp-node (:target-type e)
                               (resolve-exp (:value e) ident->symbol))
    :function-call-exp (let [fn-name (:identifier e)
                             args (:arguments e)]
                         (if (contains? ident->symbol fn-name)
                           (p/function-call-exp-node (:new-name (get ident->symbol fn-name))
                                                     (mapv #(resolve-exp % ident->symbol) args))
                           (exc/analyzer-error "Undeclared function." {:function-name fn-name})))
    (exc/analyzer-error "Invalid expression." {:exp e})))

(defn- resolve-optional-exp [e ident->symbol]
  (if (nil? e)
    e
    (resolve-exp e ident->symbol)))

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
  [{:keys [identifier initial variable-type storage-class] :as declaration} ident->symbol]
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
        {:declaration (p/variable-declaration-node unique-name storage-class variable-type init-value)
         :ident->symbol updated-symbols}))))

(defn- resolve-variable-declaration
  "Resolves variable declarations.

  Ensures variable not declared twice in the current scope."
  [decl {:keys [at-top-level] :as ident->symbol}]
  (if at-top-level
    (resolve-file-scope-variable-declaration decl ident->symbol)
    (resolve-local-variable-declaration decl ident->symbol)))

(defn- resolve-parameter [parameter ident->symbol]
  (if (and (contains? ident->symbol parameter)
           (:from-current-scope (get ident->symbol parameter)))
    (exc/analyzer-error "Parameter name duplicated." {:parameter parameter})
    (let [unique-name (unique-identifier parameter)
          updated-identifier-map (assoc ident->symbol parameter {:name unique-name
                                                                 :from-current-scope true
                                                                 :has-linkage false})]
      {:parameter unique-name
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
  [{:keys [identifier storage-class parameters function-type body] :as d} ident->symbol]
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
    {:declaration (p/function-declaration-node function-type storage-class identifier new-params (:block new-body))
     :ident->symbol updated-identifier-map}))

(defn- resolve-declaration [{:keys [declaration-type] :as d} ident->symbol]
  (condp = declaration-type
    :variable (resolve-variable-declaration d ident->symbol)
    :function (resolve-function-declaration d ident->symbol)
    (exc/analyzer-error "Invalid declaration type" {:declaration d})))

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
    :declaration (let [{d :declaration
                        i->s :ident->symbol} (resolve-declaration item ident->symbol)]
                   {:block-item d
                    :ident->symbol i->s})
    :statement {:block-item (resolve-statement item ident->symbol)
                :ident->symbol ident->symbol}))

(defn- resolve-block
  "Resolves a block under a given symbol table.

  Block is list of block items.

  ident->symbol holds identifier to symbol mapping.
  Symbol contains the type information, generated variable name etc.

  | key            | description |
  |----------------|-------------|
  |`:at-top-level` | Is current level top or not ( default true)|"
  ([block]
   (resolve-block block {:at-top-level true}))
  ([block ident->symbol]
   (let [reduce-f (fn [acc block-item]
                    (let [res (resolve-block-item block-item (:ident->symbol acc))]
                      {:block (conj (:block acc) (:block-item res))
                       :ident->symbol (:ident->symbol res)}))]
     (reduce reduce-f
             {:block []
              :ident->symbol ident->symbol}
             block))))

;; Program is list of block items, which are themselves just blocks.
(defn resolve-program [program]
  (let [res (:block (resolve-block program))
        _ (m/coerce s/Program res)]
    res))

(comment

  (def file-path "./test-programs/example.c")

  (slurp "./test-programs/example.c")

  (-> file-path
      slurp
      p/parse-from-src)

  (-> file-path
      slurp
      p/parse-from-src
      resolve-program)

  (pretty/explain
   s/Program
   (-> file-path
       slurp
       p/parse-from-src
       resolve-program))

  ())
