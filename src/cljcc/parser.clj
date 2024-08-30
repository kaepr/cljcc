(ns cljcc.parser
  (:require
   [cljcc.lexer :as l]
   [cljcc.token :as t]
   [clojure.pprint :as pp]))

(declare parse parse-exp parse-statement parse-block)

(defn- parse-repeatedly [tokens parse-f end-kind]
  (loop [tokens tokens
         res []]
    (if (= end-kind (:kind (first tokens)))
      [res tokens]
      (let [[node rst] (parse-f tokens)]
        (recur rst (conj res node))))))

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

(defn- parse-factor [[{kind :kind :as token} :as tokens]]
  (cond
    (= kind :number) [(constant-exp-node (:literal token)) (rest tokens)]
    (t/unary-op? kind) (let [op kind
                             [e rst] (parse-factor (rest tokens))]
                         [(unary-exp-node op e) rst])
    (= kind :left-paren) (let [[e rst] (parse-exp (rest tokens))
                               [_ rst] (expect :right-paren rst)]
                           [e rst])
    (= kind :identifier) [(variable-exp-node (:literal token)) (rest tokens)]
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

(defn return-statement-node [e]
  {:type :statement
   :statement-type :return
   :value e})

(defn expression-statement-node [e]
  {:type :statement
   :statement-type :expression
   :value e})

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
    (= kind :left-curly) (parse-compound-statement tokens)
    :else (parse-expression-statement tokens)))

(defn declaration-node
  ([identifier] {:type :declaration
                 :identifier identifier})
  ([identifier v] {:type :declaration
                   :identifier identifier
                   :initial v}))

(defn- parse-declaration [tokens]
  (let [[_ rst] (expect :kw-int tokens)
        [ident-token rst] (expect :identifier rst)
        [{kind :kind} :as tokens] rst]
    (cond
      (= kind :semicolon) (let [[_ rst] (expect :semicolon tokens)]
                            [(declaration-node (:literal ident-token)) rst])
      (= kind :assignment) (let [[_ rst] (expect :assignment tokens)
                                 [exp-node rst] (parse-exp rst)
                                 [_ rst] (expect :semicolon rst)]
                             [(declaration-node (:literal ident-token) exp-node) rst])
      :else (throw (ex-info "Parser error. Declaration error parsing." {})))))

(defn- parse-block-item [[token :as tokens]]
  (if (= :kw-int (:kind token))
    (parse-declaration tokens)
    (parse-statement tokens)))

(defn- parse-block [tokens]
  (let [[_ tokens] (expect :left-curly tokens)
        [block-items tokens] (parse-repeatedly tokens parse-block-item :right-curly)
        [_ tokens] (expect :right-curly tokens)]
    [block-items tokens]))

(defn- parse-function [tokens]
  (let [[fn-type-token rst] (expect :kw-int tokens)
        [fn-identifier-token rst] (expect :identifier rst)
        [_ rst] (expect :left-paren rst)
        [fn-parameter-token rst] (expect :kw-void rst)
        [_ rst] (expect :right-paren rst)
        [block-items rst] (parse-block rst)]
    [{:type :function
      :return-type (keyword->type (:kind fn-type-token))
      :identifier (:literal fn-identifier-token)
      :parameters (:kind fn-parameter-token)
      :body block-items}
     rst]))

(defn- parse-program [tokens]
  (let [[ast rst] (parse-function tokens)
        _ (expect :eof rst)]
    [ast]))

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
int a = 1;
 {
int a = 2;
 {
int a = 4;
}
}
return 0;
  }"))

  (pp/pprint
   (l/lex
    "int main(void) {return 1 + 2;}"))

  (pp/pprint
   (l/lex "
  int main(void) {
    return 2;
  }"))

  (parse "int main(void) {
   return -(((((10)))));
   }")

  (pp/pprint (parse "int main(void) {
   return 1 & 2 + 6 & 6;
   }"))

  ())
