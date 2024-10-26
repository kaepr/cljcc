(ns cljcc.parser
  (:require
   [cljcc.lexer :as l]
   [cljcc.token :as t]
   [clojure.pprint :as pp]))

(declare parse parse-exp parse-statement parse-block expect parse-declaration parse-variable-declaration)

(defn- parse-repeatedly [tokens parse-f end-kind]
  (loop [tokens tokens
         res []]
    (if (= end-kind (:kind (first tokens)))
      [res tokens]
      (let [[node rst] (parse-f tokens)]
        (recur rst (conj res node))))))

(defn- parse-optional-expression [[{kind :kind} :as tokens] parse-f end-kind]
  (if (= kind end-kind)
    (let [[_ tokens] (expect end-kind tokens)]
      [nil tokens])                     ; end kind seen, so expression not found
    (let [[e tokens] (parse-f tokens)
          [_ tokens] (expect end-kind tokens)]
      [e tokens])))

(defn- keyword->type [k]
  (condp = k
    :kw-int "int"
    (throw (ex-info "Parser Error. Unsupported type." {:keyword k}))))

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

(defn- parse-argument-list [tokens]
  (let [[e-node tokens] (parse-exp tokens)
        parse-comma-argument-f (fn [tokens]
                                 (let [[_ tokens] (expect :comma tokens)
                                       [e tokens] (parse-exp tokens)]
                                   [e tokens]))
        [rest-arguments tokens] (parse-repeatedly tokens parse-comma-argument-f :right-paren)
        [_ tokens] (expect :right-paren tokens)]
    [(into [e-node] (vec rest-arguments)) tokens]))

(defn- parse-factor [[{kind :kind :as token} :as tokens]]
  (cond
    (= kind :number) [(constant-exp-node (:literal token)) (rest tokens)]
    (t/unary-op? kind) (let [op kind
                             [e rst] (parse-factor (rest tokens))]
                         [(unary-exp-node op e) rst])
    (= kind :left-paren) (let [[e rst] (parse-exp (rest tokens))
                               [_ rst] (expect :right-paren rst)]
                           [e rst])
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
  (if (= kind :kw-int)
    (parse-variable-declaration tokens)
    (parse-optional-expression tokens parse-exp :semicolon)))

(defn- parse-for-statement [tokens]
  (let [[_ tokens] (expect :kw-for tokens)
        [_ tokens] (expect :left-paren tokens)
        [for-init-node tokens] (parse-for-init-statement tokens)
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

(defn variable-declaration-node
  ([identifier]
   {:type :declaration
    :declaration-type :variable
    :identifier identifier})
  ([identifier v]
   {:type :declaration
    :declaration-type :variable
    :identifier identifier
    :initial v}))

(defn function-declaration-node
  ([return-type identifier params]
   {:type :declaration
    :return-type return-type
    :declaration-type :function
    :identifier identifier
    :parameters params})
  ([return-type identifier params body]
   {:type :declaration
    :return-type return-type
    :declaration-type :function
    :identifier identifier
    :parameters params
    :body body}))

(defn- parse-param-list [tokens]
  (let [void? (= :kw-void (:kind (first tokens)))]
    (if void?
      (let [[_ tokens] (expect :kw-void tokens)
            [_ tokens] (expect :right-paren tokens)]
        [[] tokens]) ; void means no parameters
      (let [[_ tokens] (expect :kw-int tokens)
            [ident-token tokens] (expect :identifier tokens)
            parse-comma-f (fn [tokens]
                            (let [[_ tokens] (expect :comma tokens)
                                  [_ tokens] (expect :kw-int tokens)
                                  [ident-token tokens] (expect :identifier tokens)]
                              [ident-token tokens]))
            [rest-params tokens] (parse-repeatedly tokens parse-comma-f :right-paren)
            [_ tokens] (expect :right-paren tokens)
            map-param-f (fn [p]
                          {:parameter-name (:literal p)
                           :identifier (:literal p)
                           :parameter-type (:kind p)})
            params (map map-param-f (into [ident-token] (vec rest-params)))]
        [params tokens]))))

(defn- parse-function-declaration [tokens]
  (let [[{ret-kind :kind} tokens] (expect :kw-int tokens)
        [{fn-name :literal} tokens] (expect :identifier tokens)
        [_ tokens] (expect :left-paren tokens)
        [params tokens] (parse-param-list tokens)
        semicolon? (= :semicolon (:kind (first tokens)))]
    (if semicolon?
      (let [[_ tokens] (expect :semicolon tokens)]
        [(function-declaration-node (keyword->type ret-kind) fn-name params) tokens])
      (let [[body tokens] (parse-block tokens)]
        [(function-declaration-node (keyword->type ret-kind) fn-name params body) tokens]))))

(defn- parse-variable-declaration [tokens]
  (let [[_ tokens] (expect :kw-int tokens)
        [ident-token tokens] (expect :identifier tokens)
        [{kind :kind} :as tokens] tokens]
    (cond
      (= kind :semicolon) (let [[_ tokens] (expect :semicolon tokens)]
                            [(variable-declaration-node (:literal ident-token)) tokens])
      (= kind :assignment) (let [[_ tokens] (expect :assignment tokens)
                                 [exp-node tokens] (parse-exp tokens)
                                 [_ tokens] (expect :semicolon tokens)]
                             [(variable-declaration-node (:literal ident-token) exp-node) tokens])
      :else (throw (ex-info "Parser error. Not able  to parse variable declaration." {})))))

(defn- parse-declaration [tokens]
  (let [fn? (= :left-paren (:kind (nth tokens 2)))]
    (if fn?
      (parse-function-declaration tokens)
      (parse-variable-declaration tokens))))

(defn- parse-block-item [[token :as tokens]]
  (if (= :kw-int (:kind token))
    (parse-declaration tokens)
    (parse-statement tokens)))

(defn- parse-block [tokens]
  (let [[_ tokens] (expect :left-curly tokens)
        [block-items tokens] (parse-repeatedly tokens parse-block-item :right-curly)
        [_ tokens] (expect :right-curly tokens)]
    [block-items tokens]))

(defn- parse-program [tokens]
  (let [[fn-declaratrions tokens] (parse-repeatedly tokens parse-function-declaration :eof)
        _ (expect :eof tokens)]
    fn-declaratrions))

(defn parse [tokens]
  (-> tokens
      :tokens
      parse-program))

(defn parse-from-src [src]
  (-> src
      l/lex
      parse))

(comment

  (pp/pprint (parse-from-src "
int main(void) {
if (1 < 0) {
int x = 1;
}
}
  "))

  ())
