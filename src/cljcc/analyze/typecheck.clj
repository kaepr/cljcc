(ns cljcc.analyze.typecheck
  (:require [malli.core :as m]
            [malli.dev.pretty :as pretty]
            [cljcc.parser :as p]
            [cljcc.token :as t]
            [cljcc.exception :as exc]))

(declare typecheck-block typecheck-declaration)

(def FunAttribute
  [:map
   [:type [:= :fun]]
   [:defined? boolean?]
   [:global? boolean?]])

(def LocalAttribute
  [:map
   [:type [:= :local]]])

(def StaticAttribute
  [:map
   [:type [:= :static]]
   [:global? boolean?]])

(def Attribute
  [:multi {:dispatch :type}
   [:fun #'FunAttribute]
   [:static #'StaticAttribute]
   [:local #'LocalAttribute]])

(def Symbol
  [:map
   [:type #'p/Type]
   [:attribute #'Attribute]])

(defn- set-type
  "Assocs onto an expression given type."
  [e t] (assoc e :value-type t))

(defn- get-type [e] (:value-type e))

(defn- symbol-function? [s]
  (= :function (:type (:type s))))

(defmulti typecheck-exp
  "Returns the expression, after typechecking nested expressions."
  (fn [{:keys [exp-type]} _ident->symbol] exp-type))

(defmethod typecheck-exp :constant-exp
  [{:keys [value] :as e} _]
  (condp = (:type value)
    :int (set-type e {:type :int})
    :long (set-type e {:type :long})))

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
  [{:keys [unary-operator value]} ident->symbol]
  (let [typed-inner-e (typecheck-exp value ident->symbol)
        unary-exp (p/unary-exp-node unary-operator typed-inner-e)]
    (condp = unary-operator
      :logical-not (set-type unary-exp {:type :int})
      (set-type unary-exp (get-type typed-inner-e)))))

(defn- get-common-type [t1 t2]
  (if (= t1 t2)
    t1
    {:type :long}))

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
           common-type (get-common-type tl tr)
           convert-left-exp (convert-to-exp typed-left-e common-type)
           convert-right-exp (convert-to-exp typed-right-e common-type)
           typed-binary-exp (p/binary-exp-node convert-left-exp convert-right-exp binary-operator)]
       (if (t/arithmetic? binary-operator)
         (set-type typed-binary-exp common-type)
         (set-type typed-binary-exp {:type :int}))))))

(defmethod typecheck-exp :assignment-exp
  [{:keys [left right assignment-operator] :as e} ident->symbol]
  (let
   [typed-left (typecheck-exp left ident->symbol)
    typed-right (typecheck-exp right ident->symbol)
    left-type (get-type typed-left)
    converted-right (convert-to-exp typed-right left-type)
    typed-assign-exp (p/assignment-exp-node typed-left converted-right assignment-operator)]
   (set-type typed-assign-exp left-type)))

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
    {:statement (merge stmt (p/while-statement-node typed-cond typed-body))
     :ident->symbol (:ident->symbol typed-body)}))

(defmethod typecheck-statement :do-while
  [return-type {:keys [condition body] :as stmt} m]
  (let [typed-cond (typecheck-exp condition m)
        typed-body (typecheck-statement return-type body m)]
    {:statement (merge stmt (p/do-while-statement-node typed-cond typed-body))
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
        t-condition (typecheck-optional-expression condition m')
        t-post (typecheck-optional-expression post m')
        typed-body-statement (typecheck-statement return-type body m')]
    {:statement (merge stmt
                       (p/for-statement-node f-init t-condition t-post (:statement typed-body-statement)))
     :ident->symbol (:ident->symbol typed-body-statement)}))

(defmethod typecheck-statement :if
  [return-type {:keys [condition then-statement else-statement]} m])

(defmethod typecheck-statement :compound
  [return-type {:keys [block]} m]
  (let [typed-block (typecheck-block return-type block m)]
    {:statement (p/compound-statement-node typed-block)
     :ident->symbol (:ident->symbol typed-block)}))

(defn- typecheck-block [return-type block ident->symbol])

(defn- typecheck-declaration [])

(defn- typecheck-program [program]
  ())

(defn typecheck
  "Typechecks given program.

  Program := [Block]"
  [program])

(comment

  ())
