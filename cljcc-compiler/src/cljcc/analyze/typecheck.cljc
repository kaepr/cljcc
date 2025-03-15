(ns cljcc.analyze.typecheck
  (:require [malli.core :as m]
            [malli.dev.pretty :as pretty]
            [cljcc.parser :as p]
            [cljcc.token :as t]
            [cljcc.schema :as s]
            [cljcc.symbol :as sym]
            [clojure.core.match :refer [match]]
            [cljcc.analyze.resolve :as r]
            [cljcc.analyze.label-loops :as l]
            [cljcc.exception :as exc]
            [cljcc.util :as u]))

(declare typecheck-block typecheck-declaration to-static-init)

(defn set-type
  "Assocs onto an expression given type."
  [e t] (assoc e :value-type t))

(defn get-type [e] (:value-type e))

(defn- symbol-function? [s]
  (= :function (:type (:type s))))

(defmulti typecheck-exp
  "Returns the expression, after typechecking nested expressions."
  (fn [{:keys [exp-type]} _ident->symbol] exp-type))

(defmethod typecheck-exp :constant-exp
  [{:keys [value] :as e} _]
  (condp = (:type value)
    :int (set-type e {:type :int})
    :long (set-type e {:type :long})
    :uint (set-type e {:type :uint})
    :ulong (set-type e {:type :ulong})
    :double (set-type e {:type :double})
    (exc/analyzer-error "Invalid type for constant expression." {:e e})))

(defmethod typecheck-exp :variable-exp
  [{:keys [identifier] :as e} ident->symbol]
  (let [s (get ident->symbol identifier)]
    (if (symbol-function? s)
      (exc/analyzer-error "Function name used as variable." {:expression e})
      (set-type e (:type s)))))

(defmethod typecheck-exp :cast-exp
  [{:keys [target-type value]} ident->symbol]
  (let [typed-inner-e (typecheck-exp value ident->symbol)
        cast-exp (p/cast-exp-node target-type typed-inner-e)]
    (set-type cast-exp target-type)))

(defmethod typecheck-exp :unary-exp
  [{:keys [unary-operator value] :as e} ident->symbol]
  (let [typed-inner-e (typecheck-exp value ident->symbol)
        _ (when (and (= unary-operator :complement) (= {:type :double} (get-type typed-inner-e)))
            (exc/analyzer-error "Can't take bitwise complement of double" {:expression e}))
        unary-exp (p/unary-exp-node unary-operator typed-inner-e)]
    (condp = unary-operator
      :logical-not (set-type unary-exp {:type :int})
      (set-type unary-exp (get-type typed-inner-e)))))

(defn- get-common-type [t1 t2]
  (cond
    (= t1 t2) t1
    (or (= t1 {:type :double})
        (= t2 {:type :double})) {:type :double}
    (= (u/get-type-size t1)
       (u/get-type-size t2)) (if (u/type-signed? t1)
                               t2
                               t1)
    (> (u/get-type-size t1)
       (u/get-type-size t2)) t1
    :else t2))

(defn- convert-to-exp
  "Returns expression, using casting if necessary."
  [e t]
  (if (= (get-type e) t)
    e
    (set-type (p/cast-exp-node t e) t)))

(defmethod typecheck-exp :binary-exp
  [{:keys [left right binary-operator] :as e} ident->symbol]
  (let
   [typed-left-e (typecheck-exp left ident->symbol)
    typed-right-e (typecheck-exp right ident->symbol)]
    (if (t/logical? binary-operator)
      (set-type (p/binary-exp-node typed-left-e
                                   typed-right-e
                                   binary-operator)
                {:type :int})
      (let [tl (get-type typed-left-e)
            tr (get-type typed-right-e)
            _ (when (and (= :remainder binary-operator)
                         (or (= {:type :double} tl)
                             (= {:type :double} tr)))
                (exc/analyzer-error "Operands to remainder operation cannot be double." {:expression e}))
            common-type (get-common-type tl tr)
            convert-left-exp (convert-to-exp typed-left-e common-type)
            convert-right-exp (convert-to-exp typed-right-e common-type)
            typed-binary-exp (p/binary-exp-node convert-left-exp convert-right-exp binary-operator)]
        (if (t/arithmetic? binary-operator)
          (set-type typed-binary-exp common-type)
          (set-type typed-binary-exp {:type :int}))))))

(defmethod typecheck-exp :assignment-exp
  [{:keys [left right assignment-operator] :as _e} ident->symbol]
  (let
   [typed-left (typecheck-exp left ident->symbol)
    typed-right (typecheck-exp right ident->symbol)
    left-type (get-type typed-left)
    converted-right (convert-to-exp typed-right left-type)
    typed-assign-exp (p/assignment-exp-node typed-left converted-right assignment-operator)]
    (set-type typed-assign-exp left-type)))

(defmethod typecheck-exp :conditional-exp
  [{:keys [left right middle] :as _e} m]
  (let [t-left (typecheck-exp left m)
        t-right (typecheck-exp right m)
        t-middle (typecheck-exp middle m)
        common-type (get-common-type (get-type t-middle) (get-type t-right))
        convert-right (convert-to-exp t-right common-type)
        convert-middle (convert-to-exp t-middle common-type)
        typed-cond-e (p/conditional-exp-node t-left convert-middle convert-right)]
    (set-type typed-cond-e common-type)))

(defmethod typecheck-exp :function-call-exp
  [{:keys [identifier arguments] :as e} ident->symbol]
  (let
   [{ftype :type :as symbol} (get ident->symbol identifier)]
    (if (symbol-function? symbol)
      (let [_ (when (not= (count arguments) (count (:parameter-types ftype)))
                (exc/analyzer-error "Function called with wrong number of arguments."
                                    {:expected (count (:parameter-types ftype))
                                     :actual (count arguments)}))
            cast-arg-to-param-type-f (fn [param-type arg]
                                       (convert-to-exp (typecheck-exp arg ident->symbol)
                                                       param-type))
            converted-args (mapv cast-arg-to-param-type-f
                                 (:parameter-types ftype)
                                 arguments)
            typed-fun-call-exp (p/function-call-exp-node identifier converted-args)]
        (set-type typed-fun-call-exp (:return-type ftype)))
      (exc/analyzer-error "Variable used as function name" {:symbol symbol
                                                            :expression e}))))

(defmulti typecheck-statement
  "Dispatches based on type of statement.

  Parameters:
  - return-type: Return type of statement's enclosing function.
  - statement
  - ident->symbol: Symbol map for current scope."
  (fn [_return-type {:keys [statement-type]} _ident->symbol]
    statement-type))

(defmethod typecheck-statement :return
  [return-type {:keys [value]} ident->symbol]
  {:statement (p/return-statement-node
               (convert-to-exp (typecheck-exp value ident->symbol)
                               return-type))
   :ident->symbol ident->symbol})

(defmethod typecheck-statement :expression
  [_ {:keys [value]} ident->symbol]
  {:statement (p/expression-statement-node (typecheck-exp value ident->symbol))
   :ident->symbol ident->symbol})

(defmethod typecheck-statement :break
  [_ s m]
  {:statement s
   :ident->symbol m})

(defmethod typecheck-statement :continue
  [_ s m]
  {:statement s
   :ident->symbol m})

(defmethod typecheck-statement :empty
  [_ s m]
  {:statement s
   :ident->symbol m})

(defmethod typecheck-statement :while
  [return-type {:keys [condition body] :as stmt} m]
  (let [typed-cond (typecheck-exp condition m)
        typed-body (typecheck-statement return-type body m)]
    {:statement (merge stmt (p/while-statement-node
                             typed-cond
                             (:statement typed-body)))
     :ident->symbol (:ident->symbol typed-body)}))

(defmethod typecheck-statement :do-while
  [return-type {:keys [condition body] :as stmt} m]
  (let [typed-cond (typecheck-exp condition m)
        typed-body (typecheck-statement return-type body m)]
    {:statement (merge stmt (p/do-while-statement-node
                             typed-cond
                             (:statement typed-body)))
     :ident->symbol (:ident->symbol typed-body)}))

(defn- typecheck-optional-expression [e m]
  (if (nil? e)
    e
    (typecheck-exp e m)))

(defn- typecheck-for-init [for-init ident->symbol]
  (if (= (:type for-init) :declaration)
    (typecheck-declaration for-init ident->symbol)
    (typecheck-optional-expression for-init ident->symbol)))

(defmethod typecheck-statement :for
  [return-type {:keys [init post condition body] :as stmt} m]
  (let [f-init (typecheck-for-init init m)
        m' (if (:declaration f-init)
             (:ident->symbol f-init)
             m)
        f-init (if (:declaration f-init)
                 (:declaration f-init)
                 f-init)
        t-condition (typecheck-optional-expression condition m')
        t-post (typecheck-optional-expression post m')
        typed-body-statement (typecheck-statement return-type body m')]
    {:statement (merge stmt
                       (p/for-statement-node f-init t-condition t-post (:statement typed-body-statement)))
     :ident->symbol (:ident->symbol typed-body-statement)}))

(defmethod typecheck-statement :if
  [return-type {:keys [condition then-statement else-statement]} m]
  (if else-statement
    (let [t-condition (typecheck-exp condition m)
          {t-then :statement
           m :ident->symbol} (typecheck-statement return-type then-statement m)
          {t-else :statement
           m :ident->symbol} (typecheck-statement return-type else-statement m)]
      {:statement (p/if-statement-node t-condition t-then t-else)
       :ident->symbol m})
    (let [t-condition (typecheck-exp condition m)
          {t-then :statement
           m :ident->symbol} (typecheck-statement return-type then-statement m)]
      {:statement (p/if-statement-node t-condition t-then)
       :ident->symbol m})))

(defmethod typecheck-statement :compound
  [return-type {:keys [block]} m]
  (let [typed-block (typecheck-block return-type block m)]
    {:statement (p/compound-statement-node (:block typed-block))
     :ident->symbol (:ident->symbol typed-block)}))

(defn- typecheck-item [return-type {:keys [type] :as item} m]
  (condp = type
    :declaration (let [v (typecheck-declaration item m)]
                   {:block-item (:declaration v)
                    :ident->symbol (:ident->symbol v)})
    :statement (let [v (typecheck-statement return-type item m)]
                 {:block-item (:statement v)
                  :ident->symbol (:ident->symbol v)})
    (exc/analyzer-error "Invalid statement/declaration." item)))

(defn- typecheck-block [return-type block ident->symbol]
  (reduce (fn [acc item]
            (let [v (typecheck-item return-type item (:ident->symbol acc))]
              {:block (conj (:block acc) (:block-item v))
               :ident->symbol (:ident->symbol v)}))
          {:block []
           :ident->symbol ident->symbol}
          block))

(defn- get-initial-value
  [{:keys [initial storage-class variable-type] :as declaration}]
  (let [constant-exp? (= :constant-exp (:exp-type initial))]
    (cond
      constant-exp? (to-static-init initial variable-type)
      (nil? initial) (if (= :extern storage-class)
                       (sym/no-initializer-iv)
                       (sym/tentative-iv))
      :else (exc/analyzer-error "Non-constant initializer." declaration))))

(defn- const-convert
  "Converts a constant initializer to a specific variable type.

  Does type conversion if necessary."
  [{ttype :type :as target-type} {const-type :type value :value :as const}]
  (match [ttype const-type]
    [:double :ulong] {:type :double
                      :value (-> value
                                 biginteger
                                 (.doubleValue))}
    [:double _] {:type :double
                 :value (double value)}
    [:ulong :double] {:type :ulong
                      :value (-> value
                                 biginteger
                                 (.longValue))}
    [(:or :int :uint) _] {:type ttype
                          :value (-> value
                                     unchecked-int
                                     long)}
    [(:or :long :ulong) _] {:type ttype
                            :value (long value)}
    :else (exc/analyzer-error "Invalid type passed to const-convert function."
                              {:const const
                               :target-type target-type})))

(defn- zero-initializer
  "Returns zero const initializer based on passed type."
  [{:keys [type] :as _t}]
  (condp = type
    :int (sym/int-init 0)
    :uint (sym/uint-init 0)
    :long (sym/long-init 0)
    :ulong (sym/ulong-init 0)
    :double (sym/double-init (double 0))))

(defn- to-static-init [{:keys [value exp-type] :as e} var-type]
  (cond
    (= :constant-exp exp-type) (let [{const-type :type
                                      const-value :value} (const-convert var-type value)]
                                 (condp = const-type
                                   :int (sym/initial-iv (sym/int-init const-value))
                                   :long (sym/initial-iv (sym/long-init const-value))
                                   :uint (sym/initial-iv (sym/uint-init const-value))
                                   :ulong (sym/initial-iv (sym/ulong-init const-value))
                                   :double (sym/initial-iv (sym/double-init const-value))))
    (nil? e) (sym/initial-iv (zero-initializer var-type))
    :else (exc/analyzer-error "Non-constant initializer on static variable." e)))

(defn- validate-file-scope-variable-declaration
  [{:keys [variable-type storage-class] :as cur-decl} prev-symbol]
  (let [_ (when (not= variable-type (:type prev-symbol))
            (exc/analyzer-error "Redeclared with different types." {:declaration1 cur-decl
                                                                    :declaration2 prev-symbol}))
        global? (not= :static storage-class)
        global? (cond
                  (= :extern storage-class) (get-in prev-symbol [:attribute :global?])
                  (not= global? (get-in prev-symbol [:attribute :global?])) (exc/analyzer-error "Conflicting variable linkage." {:d1 cur-decl
                                                                                                                                 :d2 prev-symbol})
                  :else global?)
        initial-value (get-initial-value cur-decl)
        initial-value (cond
                        (=
                         :initial
                         (get-in prev-symbol [:attribute :initial-value :type])) (if (= (:type initial-value) :initial)
                                                                                   (exc/analyzer-error "Conflicting file scope variable definition." {:d1 cur-decl
                                                                                                                                                      :d2 prev-symbol})
                                                                                   (get-in prev-symbol [:attribute :initial-value]))
                        (and
                         (= :tentative (get-in prev-symbol [:attribute :initial-value :type]))
                         (not= :initial (:type initial-value))) {:type :tentative}
                        :else initial-value)]
    {:global? global?
     :initial-value initial-value}))

(defn- typecheck-file-scope-variable-declaration
  [{:keys [identifier storage-class variable-type] :as d} ident->symbol]
  (let [prev-symbol (get ident->symbol identifier)
        global? (not= :static storage-class)
        initial-value (get-initial-value d)
        {global? :global?
         initial-value :initial-value} (if prev-symbol
                                         (validate-file-scope-variable-declaration d prev-symbol)
                                         {:global? global?
                                          :initial-value initial-value})]
    {:declaration d
     :ident->symbol (assoc ident->symbol
                           identifier
                           (sym/create-symbol variable-type (sym/static-attribute initial-value global?)))}))

(defn- typecheck-local-scope-variable-declaration
  [{:keys [identifier variable-type storage-class initial] :as d} ident->symbol]
  (condp = storage-class
    :extern (let [_ (when (not (nil? initial))
                      (exc/analyzer-error "Initializer on local extern variable declaration." d))
                  prev-symbol (get ident->symbol identifier)
                  prev-type (:type prev-symbol)
                  _ (when (and prev-symbol (not= prev-type variable-type))
                      (exc/analyzer-error "Redeclared with different types." {:declaration1 d
                                                                              :declaration2 prev-symbol}))
                  symbols (if prev-symbol
                            ident->symbol
                            (assoc ident->symbol
                                   identifier
                                   (sym/create-symbol variable-type (sym/static-attribute (sym/no-initializer-iv) true))))]
              {:declaration d
               :ident->symbol symbols})
    :static (let [initial-value (to-static-init initial variable-type)
                  updated-symbols (assoc ident->symbol
                                         identifier
                                         (sym/create-symbol variable-type (sym/static-attribute initial-value false)))]
              {:declaration d
               :ident->symbol updated-symbols})
    (let [updated-symbols (assoc ident->symbol
                                 identifier
                                 (sym/create-symbol
                                  variable-type
                                  (sym/local-attribute)))
          casted-e (if (nil? initial)
                     initial
                     (convert-to-exp initial variable-type))
          t-e (typecheck-optional-expression casted-e updated-symbols)]
      {:declaration (assoc d :initial t-e)
       :ident->symbol updated-symbols})))

(defn- validate-old-fn-decl-return-attribute
  [cur-decl prev-symbol]
  (let [prev-function? (= :function (get-in prev-symbol [:type :type]))
        _ (when-not prev-function?
            (exc/analyzer-error "Variable being redeclared as function." {:declaration cur-decl
                                                                          :prev-symbol prev-symbol}))
        same-type? (and (= (get-in cur-decl [:function-type :parameter-types])
                           (get-in prev-symbol [:type :parameter-types]))
                        (= (get-in cur-decl [:function-type :return-type])
                           (get-in prev-symbol [:type :return-type])))
        _ (when-not same-type?
            (exc/analyzer-error "Incompatible function type declarations." {:declaration cur-decl
                                                                            :prev-declaration-type prev-symbol}))
        defined? (seq (:body cur-decl))
        prev-defined? (get-in prev-symbol [:attribute :defined?])
        _ (when (and defined? prev-defined?)
            (exc/analyzer-error "Function defined more than once." {:declaration cur-decl}))
        current-static? (= :static (:storage-class cur-decl))
        old-global? (get-in prev-symbol [:attribute :global?])
        _ (when (and old-global? current-static?)
            (exc/analyzer-error "Static function definition follows non static." {:declaration cur-decl}))]
    {:defined? prev-defined?
     :global? old-global?}))

(defn- add-parameter-to-symbols
  [parameters function-type ident->symbol]
  (if (zero? (count parameters))
    ident->symbol
    (apply assoc
           ident->symbol
           (flatten
            (map (fn [p t]
                   [p (sym/create-symbol t (sym/local-attribute))])
                 parameters
                 (:parameter-types function-type))))))

(defn- typecheck-function-declaration
  [{:keys [identifier storage-class body parameters function-type] :as d} ident->symbol]
  (let [body? (seq body)
        prev-symbol (get ident->symbol identifier)
        {defined? :defined?
         global? :global?} (if prev-symbol
                             (validate-old-fn-decl-return-attribute d prev-symbol)
                             {:defined? false
                              :global? (not= :static storage-class)})
        function-attribute (sym/fun-attribute (boolean (or defined? body?)) global?)
        updated-symbols (assoc ident->symbol
                               identifier
                               (sym/create-symbol
                                function-type
                                function-attribute))]
    (if body?
      (let [with-parameter-symbols (add-parameter-to-symbols
                                    parameters
                                    function-type
                                    updated-symbols)
            with-body-symbols (typecheck-block (:return-type function-type)
                                               body
                                               (assoc with-parameter-symbols
                                                      :at-top-level false))]
        {:declaration (assoc d :body (:block with-body-symbols))
         :ident->symbol (assoc (:ident->symbol with-body-symbols)
                               :at-top-level true)})
      {:declaration d
       :ident->symbol updated-symbols})))

(defn- typecheck-declaration
  [{:keys [declaration-type] :as d} ident->symbol]
  (let [at-top-level? (:at-top-level ident->symbol)]
    (condp = declaration-type
      :variable (if at-top-level?
                  (typecheck-file-scope-variable-declaration d ident->symbol)
                  (typecheck-local-scope-variable-declaration d ident->symbol))
      :function (typecheck-function-declaration d ident->symbol)
      (exc/analyzer-error "Invalid declaration for typechecker." {:declaration d}))))

(defn- typecheck-program [program]
  (let [rf (fn [acc decl]
             (let [d (typecheck-declaration decl (:ident->symbol acc))]
               {:program (conj (:program acc) (:declaration d))
                :ident->symbol (:ident->symbol d)}))]
    (reduce rf
            {:program []
             :ident->symbol {:at-top-level true}}
            program)))

(defn typecheck
  "Typechecks given program.

  A program is a list of declarations."
  [program]
  (let [v (typecheck-program program)
        program (:program v)
        m (dissoc (:ident->symbol v) :at-top-level)
        ;_ (m/coerce s/Program program)
        ;_ (m/coerce s/SymbolMap m)
        ]
    {:program program
     :ident->symbol m}))

(comment

  (def file-path "./test-programs/example.c")

  (slurp "./test-programs/example.c")

  (-> file-path
      slurp
      p/parse-from-src)

  (-> file-path
      slurp
      p/parse-from-src
      r/resolve-program
      l/label-loops
      typecheck)

  (->
   "unsigned long ul = 18446744073709549568.;"
   p/parse-from-src
   r/resolve-program
   l/label-loops
   typecheck)

  (pretty/explain
   s/TypecheckedOut
   (-> file-path
       slurp
       p/parse-from-src
       r/resolve-program
       l/label-loops
       typecheck))

  ())
