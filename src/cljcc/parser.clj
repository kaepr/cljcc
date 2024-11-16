(ns cljcc.parser
  (:require
   [cljcc.lexer :as l]
   [cljcc.token :as t]
   [malli.core :as m]
   [clojure.math :refer [pow]]
   [malli.dev.pretty :as pretty]
   [cljcc.exception :as exc]
   [clojure.string :as str]))

(declare Statement Exp Declaration Block Type)

(def StorageClass [:enum :static :extern])

(def IntType
  [:map
   [:type [:= :int]]])

(def LongType
  [:map
   [:type [:= :long]]])

(def FunType
 [:map
  [:type [:= :function]]
  [:return-type [:ref #'Type]]
  [:parameter-types [:vector [:ref #'Type]]]])

(def Type
  [:schema {:registry {::mtype-int #'IntType
                       ::mtype-long #'LongType
                       ::mtype-function #'FunType}}
   [:multi {:dispatch :type}
    [:int #'IntType]
    [:long #'LongType]
    [:function #'FunType]]])

(def Const
  [:map
   [:type [:enum :int :long]]
   [:value int?]])

(def ConstantExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :constant-exp]]
   [:value #'Const]])

(def VariableExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :variable-exp]]
   [:identifier string?]])

(def CastExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :cast-exp]]
   [:target-type #'Type]
   [:value [:ref #'Exp]]])

(def UnaryExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :unary-exp]]
   [:unary-operator `[:enum ~@t/unary-ops]]
   [:value [:ref #'Exp]]])

(def BinaryExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :binary-exp]]
   [:binary-operator `[:enum ~@(set (keys t/bin-ops))]]
   [:left [:ref #'Exp]]
   [:right [:ref #'Exp]]])

(def AssignmentExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :assignment-exp]]
   [:assignment-operator `[:enum ~@t/assignment-ops]]
   [:left [:ref #'Exp]]
   [:right [:ref #'Exp]]])

(def ConditionalExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :conditional-exp]]
   [:left [:ref #'Exp]]
   [:middle [:ref #'Exp]]
   [:right [:ref #'Exp]]])

(def FunctionCallExp
  [:map
   [:type [:= :exp]]
   [:exp-type [:= :function-call-exp]]
   [:identifier string?]
   [:arguments [:vector [:ref #'Exp]]]])

(def Exp
  [:schema {:registry {::mexp-constant #'ConstantExp
                       ::mexp-variable #'VariableExp
                       ::mexp-cast #'CastExp
                       ::mexp-unary #'UnaryExp
                       ::mexp-binary #'BinaryExp
                       ::mexp-assignment #'AssignmentExp

                       ::mexp-conditional #'ConditionalExp
                       ::mexp-function-call #'FunctionCallExp}}
   [:multi {:dispatch :exp-type}
    [:constant-exp #'ConstantExp]
    [:variable-exp #'VariableExp]
    [:cast-exp #'CastExp]
    [:unary-exp #'UnaryExp]
    [:binary-exp #'BinaryExp]
    [:assignment-exp #'AssignmentExp]
    [:conditional-exp #'ConditionalExp]
    [:function-call-exp #'FunctionCallExp]]])

(def VarDeclaration
  [:map
   [:type [:= :declaration]]
   [:declaration-type [:= :variable]]
   [:variable-type #'Type]
   [:storage-class [:maybe #'StorageClass]]
   [:identifier string?]
   [:initial [:maybe #'Exp]]])

(def FunDeclaration
  [:map
   [:type [:= :declaration]]
   [:declaration-type [:= :function]]
   [:function-type #'FunType]
   [:identifier string?]
   [:storage-class [:maybe #'StorageClass]]
   [:parameters [:vector string?]]
   [:body [:maybe [:ref #'Block]]]])

(def ReturnStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :return]]
   [:value #'Exp]])

(def ExpressionStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :expression]]
   [:value #'Exp]])

(def BreakStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :break]]
   [:label [:maybe string?]]])

(def ContinueStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :continue]]
   [:label [:maybe string?]]])

(def EmptyStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :empty]]])

(def WhileStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :while]]
   [:condition #'Exp]
   [:body [:ref #'Statement]]])

(def DoWhileStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :do-while]]
   [:condition #'Exp]
   [:body [:ref #'Statement]]])

(def ForStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :for]]
   [:init [:or
           [:ref #'VarDeclaration]
           [:maybe #'Exp]]]
   [:post [:maybe #'Exp]]
   [:condition [:maybe #'Exp]]
   [:body [:ref #'Statement]]])

(def IfStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :if]]
   [:condition #'Exp]
   [:then-statement [:ref #'Statement]]
   [:else-statement [:maybe [:ref #'Statement]]]])

(def CompoundStatement
  [:map
   [:type [:= :statement]]
   [:statement-type [:= :compound]]
   [:block [:ref #'Block]]])

(def Statement
  [:schema {:registry {::mstatement-return #'ReturnStatement
                       ::mstatement-expression #'ExpressionStatement
                       ::mstatement-break #'BreakStatement
                       ::mstatement-continue #'ContinueStatement
                       ::mstatement-empty #'EmptyStatement
                       ::mstatement-for #'ForStatement
                       ::mstatement-while #'WhileStatement
                       ::mstatement-do-while #'DoWhileStatement
                       ::mstatement-compound #'CompoundStatement
                       ::mstatement-if #'IfStatement}}
    [:multi {:dispatch :statement-type}
      [:return #'ReturnStatement]
      [:expression #'ExpressionStatement]
      [:break #'BreakStatement]
      [:continue #'ContinueStatement]
      [:empty #'EmptyStatement]
      [:compound #'CompoundStatement]
      [:while #'WhileStatement]
      [:do-while #'DoWhileStatement]
      [:if #'IfStatement]
      [:for #'ForStatement]]])

(def Declaration
  [:schema {:registry {::mdeclaration-function #'FunDeclaration
                       ::mdeclaration-variable #'VarDeclaration}}
   [:multi {:dispatch :declaration-type}
    [:function #'FunDeclaration]
    [:variable #'VarDeclaration]]])

(def BlockItem
  [:schema {:registry {::mblockitem-statement #'Statement
                       ::mblockitem-declaration #'Declaration}}
   [:multi {:dispatch :type}
    [:statement [:ref #'Statement]]
    [:declaration [:ref #'Declaration]]]])

(def Block
  [:schema {:registry {::mblock-blockitem #'BlockItem}}
   [:vector [:ref #'BlockItem]]])

(def Program
  [:schema {:registry {::mprogram-block #'Block}}
   [:vector [:ref #'Declaration]]])

(comment

  (pretty/explain
   Block
   [{:type :statement
     :statement-type :compound
     :block [{:type :statement
              :statement-type :return
              :value {:type :exp
                      :exp-type :variable-exp
                      :identifier "asd"}}]}])

  ())

(declare parse parse-exp parse-statement parse-block expect parse-declaration parse-variable-declaration)

(defn- parse-repeatedly
  "Repeatedly runs given parse function on input until end-kind encountered.

  `parse-f` must return result in form [node remaining-tokens]."
  [tokens parse-f end-kind]
  (loop [res []
         tokens tokens]
    (if (= end-kind (:kind (first tokens)))
      [res tokens]
      (let [[node rst] (parse-f tokens)]
        (recur (conj res node) rst)))))

(defn- parse-optional-expression [[{kind :kind} :as tokens] parse-f end-kind]
  (if (= kind end-kind)
    (let [[_ tokens] (expect end-kind tokens)]
      [nil tokens])                     ; end kind seen, so expression not found
    (let [[e tokens] (parse-f tokens)
          [_ tokens] (expect end-kind tokens)]
      [e tokens])))

(defn- expect
  "Expects the first token in list to be of given kind.

  Returns the token and remaining tokens."
  [kind [token & rst]]
  (if (= kind (:kind token))
    [token rst]
    (throw (ex-info "Parser Error." {:expected kind
                                     :actual (:kind token)}))))

(defn constant-exp-node [v]
  {:type :exp
   :exp-type :constant-exp
   :value v})

(defn variable-exp-node [identifier]
  {:type :exp
   :exp-type :variable-exp
   :identifier identifier})

(defn function-call-exp-node [identifier arguments]
  {:type :exp
   :exp-type :function-call-exp
   :identifier identifier
   :arguments arguments})

(defn cast-exp-node [target-type e]
  {:type :exp
   :exp-type :cast-exp
   :target-type target-type
   :value e})

(defn unary-exp-node [op v]
  {:type :exp
   :exp-type :unary-exp
   :unary-operator op
   :value v})

(defn binary-exp-node [l r op]
  {:type :exp
   :exp-type :binary-exp
   :binary-operator op
   :left l
   :right r})

(defn assignment-exp-node [l r op]
  {:type :exp
   :exp-type :assignment-exp
   :assignment-operator op
   :left l
   :right r})

(defn conditional-exp-node [l m r]
  {:type :exp
   :exp-type :conditional-exp
   :left l
   :middle m
   :right r})

(defn- parse-type [specifiers]
  (condp = (mapv :specifier-type specifiers)
    [:int] :int
    [:long] :long
    [:int :long] :long
    [:long :int] :long
    (exc/parser-error "Invalid specifiers" specifiers)))

(defn specifier-node [{:keys [kind] :as token}]
  (let [specifier-type (condp = kind
                         :kw-int :int
                         :kw-long :long
                         :kw-static :static
                         :kw-extern :extern
                         (exc/parser-error "Parser Error. Invalid specifier." {:specifier-token token}))]
    {:type :specifier
     :specifier-type specifier-type}))

(defn- parse-type-specifier [[{:keys [kind] :as token} & rst]]
  (if-not (contains? #{:kw-int :kw-long} kind)
    (exc/parser-error "Invalid token for type specifier" {:token token})
    [(specifier-node token) rst]))

(defn- parse-specifier [[{:keys [kind] :as token} & rst]]
  (if-not (contains? #{:kw-int :kw-long :kw-static :kw-extern} kind)
    (exc/parser-error "Invalid token for specifier" {:token token})
    [(specifier-node token) rst]))

(defn- parse-argument-list [tokens]
  (let [[e-node tokens] (parse-exp tokens)
        parse-comma-argument-f (fn [tokens]
                                 (let [[_ tokens] (expect :comma tokens)
                                       [e tokens] (parse-exp tokens)]
                                   [e tokens]))
        [rest-arguments tokens] (parse-repeatedly tokens parse-comma-argument-f :right-paren)
        [_ tokens] (expect :right-paren tokens)]
    [(into [e-node] (vec rest-arguments)) tokens]))

(defn- parse-const
  "Expects a stringified number."
  [v]
  (let [long? (or (= \l (last v))
                  (= \L (last v)))
        n (if long?
            (Long/parseLong (str/join (subvec (vec v) 0 (dec (count v)))))
            (Long/parseLong v))
        int-range? (and (not long?)
                        (<= n (- (long (pow 2 31)) 1)))]
    {:type (if int-range? :int :long)
     :value n}))

(defn- parse-factor [[{kind :kind :as token} :as tokens]]
  (cond
    (= kind :number) [(constant-exp-node (parse-const (:literal token))) (rest tokens)]
    (t/unary-op? kind) (let [op kind
                             [e rst] (parse-factor (rest tokens))]
                         [(unary-exp-node op e) rst])
    (= kind :left-paren) (let [next-token-kind (:kind (first (rest tokens)))
                               type-specifier? (contains? #{:kw-int :kw-long} next-token-kind)]
                           (if type-specifier?
                             (let [[specifiers tokens] (parse-repeatedly (rest tokens) parse-type-specifier :right-paren)
                                   ptype (parse-type specifiers)
                                   [_ tokens] (expect :right-paren tokens)
                                   [f tokens] (parse-factor tokens)]
                               [(cast-exp-node {:type ptype} f) tokens])
                             (let [[e rst] (parse-exp (rest tokens))
                                   [_ rst] (expect :right-paren rst)]
                               [e rst])))
    (= kind :identifier) (if (= :left-paren (:kind (second tokens))) ; is a fn call
                           (let [[{f-name :literal} tokens] (expect :identifier tokens)
                                 [_ tokens] (expect :left-paren tokens)
                                 right-paren? (= :right-paren (:kind (first tokens)))]
                             (if right-paren?
                               (let [[_ tokens] (expect :right-paren tokens)]
                                 [(function-call-exp-node f-name []) tokens])
                               (let [[arguments tokens] (parse-argument-list tokens)]
                                 [(function-call-exp-node f-name arguments) tokens])))
                           [(variable-exp-node (:literal token)) (rest tokens)])
    :else (throw (ex-info "Parser Error. Malformed token." {:token token}))))

(defn- parse-exp
  ([tokens]
   (parse-exp tokens 0))
  ([tokens min-prec]
   (loop [[left rst] (parse-factor tokens)
          tokens rst]
     (let [[{kind :kind :as _token} :as tokens] tokens]
       (if (and (t/binary-op? kind) (>= (t/precedence kind) min-prec))
         (cond
           (t/assignment-op? kind) (let [[_ tokens] (expect kind tokens)
                                         [right rst] (parse-exp tokens (t/precedence kind))]
                                     (recur [(assignment-exp-node left right kind)] rst))
           (= :question kind) (let [[_ tokens] (expect :question tokens)
                                    [middle tokens] (parse-exp tokens)
                                    [_ tokens] (expect :colon tokens)
                                    [right tokens] (parse-exp tokens (inc (t/precedence kind)))]
                                (recur [(conditional-exp-node left middle right)] tokens))
           :else (let [[right rst] (parse-exp (rest tokens) (inc (t/precedence kind)))]
                   (recur [(binary-exp-node left right kind)] rst)))
         [left tokens])))))

;;;; Statements

(defn return-statement-node [e]
  {:type :statement
   :statement-type :return
   :value e})

(defn expression-statement-node [e]
  {:type :statement
   :statement-type :expression
   :value e})

(defn break-statement-node
  ([] (break-statement-node nil))
  ([label]
   {:type :statement
    :statement-type :break
    :label label}))

(defn continue-statement-node
  ([] (continue-statement-node nil))
  ([label]
   {:type :statement
    :statement-type :continue
    :label label}))

(defn empty-statement-node []
  {:type :statement
   :statement-type :empty})

(defn compound-statement-node [block]
  {:type :statement
   :statement-type :compound
   :block block})

(defn if-statement-node
  ([cond then]
   (if-statement-node cond then nil))
  ([cond then else]
   {:type :statement
    :statement-type :if
    :condition cond
    :then-statement then
    :else-statement else}))

(defn while-statement-node [cond-exp body-statement]
  {:type :statement
   :statement-type :while
   :condition cond-exp
   :body body-statement})

(defn do-while-statement-node [cond-exp body-statement]
  {:type :statement
   :statement-type :do-while
   :condition cond-exp
   :body body-statement})

(defn for-statement-node [for-init cond-exp post-exp body-statement]
  {:type :statement
   :statement-type :for
   :condition cond-exp
   :post post-exp
   :init for-init
   :body body-statement})

(defn for-init-node [decl exp]
  {:type :for-initializer
   :init-declaration decl
   :init-exp exp})

;;;; Parse statement nodes

(defn- parse-return-statement [tokens]
  (let [[_ rst] (expect :kw-return tokens)
        [exp-node rst] (parse-exp rst)
        [_ rst] (expect :semicolon rst)]
    [(return-statement-node exp-node) rst]))

(defn- parse-expression-statement [tokens]
  (let [[exp-node rst] (parse-exp tokens)
        [_ rst] (expect :semicolon rst)]
    [(expression-statement-node exp-node) rst]))

(defn- parse-empty-statement
  "Parses statement expect only single semicolon"
  [tokens]
  (let [[_ rst] (expect :semicolon tokens)]
    [(empty-statement-node) rst]))

(defn- parse-break-statement [tokens]
  (let [[_ tokens] (expect :kw-break tokens)
        [_ tokens] (expect :semicolon tokens)]
    [(break-statement-node) tokens]))

(defn- parse-continue-statement [tokens]
  (let [[_ tokens] (expect :kw-continue tokens)
        [_ tokens] (expect :semicolon tokens)]
    [(continue-statement-node) tokens]))

(defn- parse-while-statement [tokens]
  (let [[_ tokens] (expect :kw-while tokens)
        [_ tokens] (expect :left-paren tokens)
        [e tokens] (parse-exp tokens)
        [_ tokens] (expect :right-paren tokens)
        [s tokens] (parse-statement tokens)]
    [(while-statement-node e s) tokens]))

(defn- parse-do-while-statement [tokens]
  (let [[_ tokens] (expect :kw-do tokens)
        [s tokens] (parse-statement tokens)
        [_ tokens] (expect :kw-while tokens)
        [_ tokens] (expect :left-paren tokens)
        [e tokens] (parse-exp tokens)
        [_ tokens] (expect :right-paren tokens)
        [_ tokens] (expect :semicolon tokens)]
    [(do-while-statement-node e s) tokens]))

(defn- parse-for-init-statement [[{kind :kind} :as tokens]]
  (if (contains? #{:kw-int :kw-static :kw-long :kw-extern} kind)
    (parse-declaration tokens)
    (parse-optional-expression tokens parse-exp :semicolon)))

(defn- parse-for-statement [tokens]
  (let [[_ tokens] (expect :kw-for tokens)
        [_ tokens] (expect :left-paren tokens)
        [for-init-node tokens] (parse-for-init-statement tokens)
        _ (when (= :function (:declaration-type for-init-node))
            (exc/parser-error "Function declaration used in initializer node." for-init-node))
        _ (when-not (nil? (:storage-class for-init-node))
            (exc/parser-error "For initializer cannot contain storage class specifier." for-init-node))
        [cond-exp tokens] (parse-optional-expression tokens parse-exp :semicolon)
        [post-exp tokens] (parse-optional-expression tokens parse-exp :right-paren)
        [stmt tokens] (parse-statement tokens)]
    [(for-statement-node for-init-node cond-exp post-exp stmt) tokens]))

(defn- parse-if-statement [tokens]
  (let [[_ tokens] (expect :kw-if tokens)
        [_ tokens] (expect :left-paren tokens)
        [exp-node tokens] (parse-exp tokens)
        [_ tokens] (expect :right-paren tokens)
        [then-stmt tokens] (parse-statement tokens)
        else? (= :kw-else (:kind (first tokens)))]
    (if (not else?)
      [(if-statement-node exp-node then-stmt) tokens]
      (let [[_ tokens] (expect :kw-else tokens)
            [else-stmt tokens] (parse-statement tokens)]
        [(if-statement-node exp-node then-stmt else-stmt) tokens]))))

(defn- parse-compound-statement [tokens]
  (let [[block-items tokens] (parse-block tokens)]
    [(compound-statement-node block-items) tokens]))

(defn- parse-statement
  "Parses a single statement. Expects a semicolon at the end."
  [[{kind :kind} :as tokens]]
  (cond
    (= kind :semicolon) (parse-empty-statement tokens)
    (= kind :kw-return) (parse-return-statement tokens)
    (= kind :kw-if) (parse-if-statement tokens)
    (= kind :kw-break) (parse-break-statement tokens)
    (= kind :kw-continue) (parse-continue-statement tokens)
    (= kind :kw-for) (parse-for-statement tokens)
    (= kind :kw-while) (parse-while-statement tokens)
    (= kind :kw-do) (parse-do-while-statement tokens)
    (= kind :left-curly) (parse-compound-statement tokens)
    :else (parse-expression-statement tokens)))

(defn parameter-node [{:keys [identifier ptype]}]
  {:parameter-name identifier
   :identifier identifier
   :parameter-type ptype})

(defn variable-declaration-node
  ([identifier storage-class vtype]
   (variable-declaration-node identifier storage-class vtype nil))
  ([identifier storage-class vtype init-exp]
   {:type :declaration
    :declaration-type :variable
    :variable-type vtype
    :storage-class storage-class
    :identifier identifier
    :initial init-exp}))

(defn function-declaration-node
  ([function-type storage-class identifier parameters]
   (function-declaration-node function-type storage-class identifier parameters nil))
  ([function-type storage-class identifier parameters body]
   {:type :declaration
    :declaration-type :function
    :function-type function-type
    :storage-class storage-class
    :identifier identifier
    :parameters parameters
    :body body}))

(defn- parse-param-list [tokens]
  (let [void? (= :kw-void (:kind (first tokens)))]
    (if void?
      (let [[_ tokens] (expect :kw-void tokens)
            [_ tokens] (expect :right-paren tokens)]
        [[] tokens]) ; void means no parameters
      (let [[specifiers tokens] (parse-repeatedly tokens parse-type-specifier :identifier)
            first-parameter-type (parse-type specifiers)
            [ident-token tokens] (expect :identifier tokens)
            parse-comma-f (fn [tokens]
                            (let [[_ tokens] (expect :comma tokens)
                                  [specifiers tokens] (parse-repeatedly tokens parse-type-specifier :identifier)
                                  ptype (parse-type specifiers)
                                  [ident-token tokens] (expect :identifier tokens)]
                              [{:identifier (:literal ident-token)
                                :ptype ptype}
                               tokens]))
            [rest-params tokens] (parse-repeatedly tokens parse-comma-f :right-paren)
            [_ tokens] (expect :right-paren tokens)
            params (mapv parameter-node (into [{:identifier (:literal ident-token)
                                                :ptype first-parameter-type}]
                                              (vec rest-params)))]
        [params tokens]))))

(defn- parse-function-declaration [return-type storage-class tokens]
  (let [[{fn-name :literal} tokens] (expect :identifier tokens)
        [_ tokens] (expect :left-paren tokens)
        [parameter-nodes tokens] (parse-param-list tokens)
        parameters (mapv :identifier parameter-nodes)
        parameter-types (mapv :parameter-type parameter-nodes)
        function-type {:type :function
                       :return-type {:type return-type}
                       :parameter-types (mapv (fn [v] {:type v}) parameter-types)}
        semicolon? (= :semicolon (:kind (first tokens)))]
    (if semicolon?
      (let [[_ tokens] (expect :semicolon tokens)]
        [(function-declaration-node function-type storage-class fn-name parameters) tokens])
      (let [[body tokens] (parse-block tokens)]
        [(function-declaration-node function-type storage-class fn-name parameters body) tokens]))))

(defn- parse-variable-declaration [variable-type storage-class tokens]
  (let [[ident-token tokens] (expect :identifier tokens)
        [{kind :kind} :as tokens] tokens
        variable-type {:type variable-type}]
    (cond
      (= kind :semicolon) (let [[_ tokens] (expect :semicolon tokens)]
                            [(variable-declaration-node (:literal ident-token) storage-class variable-type) tokens])
      (= kind :assignment) (let [[_ tokens] (expect :assignment tokens)
                                 [exp-node tokens] (parse-exp tokens)
                                 [_ tokens] (expect :semicolon tokens)]
                             [(variable-declaration-node (:literal ident-token) storage-class variable-type exp-node) tokens])
      :else (throw (ex-info "Parser error. Not able  to parse variable declaration." {})))))

(defn- parse-type-and-storage-class [specifiers]
  (let [valid-types #{:int :long}
        {types true, storage-classes false} (group-by #(contains? valid-types (:specifier-type %)) specifiers)
        type-specifier (parse-type types)
        storage-class (if (> (count storage-classes) 1)
                        (exc/parser-error "Invalid storage class." {:storage-classes storage-classes})
                        (:specifier-type (first storage-classes)))]
    {:type-specifier type-specifier
     :storage-class storage-class}))

(defn- parse-declaration [tokens]
  (let [[specifiers tokens] (parse-repeatedly tokens parse-specifier :identifier)
        {type-specifier :type-specifier, storage-class :storage-class} (parse-type-and-storage-class specifiers)
        fn? (= :left-paren (:kind (nth tokens 1)))]
    (if fn?
      (parse-function-declaration type-specifier storage-class tokens)
      (parse-variable-declaration type-specifier storage-class tokens))))

(defn- parse-block-item [[token :as tokens]]
  (if (contains? #{:kw-int :kw-static :kw-extern :kw-long} (:kind token))
    (parse-declaration tokens)
    (parse-statement tokens)))

(defn- parse-block [tokens]
  (let [[_ tokens] (expect :left-curly tokens)
        [block-items tokens] (parse-repeatedly tokens parse-block-item :right-curly)
        [_ tokens] (expect :right-curly tokens)]
    [block-items tokens]))

(defn- parse-program [tokens]
  (let [[declarations tokens] (parse-repeatedly tokens parse-declaration :eof)
        _ (expect :eof tokens)]
    declarations))

(defn parse [tokens]
  (-> tokens
      :tokens
      parse-program))

(defn parse-from-src [src]
  (-> src
      l/lex
      parse))

(comment

  (m/validate
   Program)

  (m/coerce
   Program
   (parse-from-src
    "int main(void) {
return (long) 42;
}"))


  (pretty/explain
   Program
   (parse-from-src
    "
long add(int a, int b) {
    return (long) a + (long) b;
}

int main(void) {
    long a = add(2147483645, 2147483645);
    if (a == 4294967290l) {
        return 1;
    }
    return 0;
}
"))

  (pretty/explain
   Program
   (parse-from-src
    "int main(void) {
    long l = 9223372036854775807l;
    return (l - 2l == 9223372036854775805l);
}
"))
   

  (parse-from-src "
int main(void) {

    int x = 0;

    for (int i = 0; i < 10; i = i + 1) {
        x = x + 1;
    }

    return x;
}

int foo(int x) {
x += 1;
return x;
}

")

  ())
