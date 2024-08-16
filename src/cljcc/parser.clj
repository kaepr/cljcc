(ns cljcc.parser
  (:require
   [cljcc.lexer :as l]
   [clojure.pprint :as pp]))

(defn- expect [kind [token & rst]]
  (if (= kind (:kind token))
    [token rst]
    (throw (ex-info "Parser Error." {:expected kind
                                     :actual (:kind token)}))))

(defn- expect-in [kinds [{kind :kind :as token} & rst]]
  (if (contains? kinds kind)
    [token rst]
    (throw (ex-info "Parser Error." {:expected kinds
                                     :actual token}))))

(defn- parse-exp [[{kind :kind :as token} :as tokens]]
  (cond
    (= kind :number) [{:type :exp
                       :exp-type :constant-exp
                       :value (:literal token)}
                      (rest tokens)]
    (contains?
     #{:complement :hyphen} kind) (let [operator kind
                                        [e rst] (parse-exp (rest tokens))]
                                    [{:type :exp
                                      :exp-type :unary-exp
                                      :unary-operator operator
                                      :value e}
                                     rst])
    (= kind :left-paren) (let [[e rst] (parse-exp (rest tokens))
                               [_ rst] (expect :right-paren rst)]
                           [e rst])
    :else (throw (ex-info "Parser Error." {:expected "number, (, -, ~"
                                           :actual token}))))

(defn- parse-return-statement [tokens]
  (let [[_ rst] (expect :kw-return tokens)
        [exp-node rst] (parse-exp rst)]
    [{:type :statement
      :statement-type :return
      :value exp-node}
     rst]))

(defn- parse-statement
  "Parses a single statement. Expects a semicolon at the end."
  [[token :as tokens]]
  (let [[statement rst]
        (cond
          (= (:kind token) :kw-return) (parse-return-statement tokens)
          :else (throw (ex-info "Parser Error. Unexpected statement. " {:token token})))
        [_ rst] (expect :semicolon rst)]
    [statement rst]))

(defn- keyword->type [k]
  (condp = k
    :kw-int "int"
    (throw (ex-info "Parser Error. Unsupported type." {:keyword k}))))

(defn- parse-function [tokens]
  (let [[fn-type-token rst] (expect :kw-int tokens)
        [fn-identifier-token rst] (expect :identifier rst)
        [_ rst] (expect :left-paren rst)
        [fn-parameter-token rst] (expect :kw-void rst)
        [_ rst] (expect :right-paren rst)
        [_ rst] (expect :left-curly rst)
        [statement rst] (parse-statement rst)
        [_ rst] (expect :right-curly rst)]
    [{:type :function
      :return-type (keyword->type (:kind fn-type-token))
      :identifier (:literal fn-identifier-token)
      :parameters (:kind fn-parameter-token)
      :statements [statement]}
     rst]))

(defn- parse-program [tokens]
  (let [[ast rst] (parse-function tokens)
        _ (expect :eof rst)]
    [ast]))

(defn parse [tokens]
  (-> tokens
      :tokens
      parse-program))

(comment

  (pp/pprint (parse (l/lex "
  int main(void) {
  return -(~~~(2));
  }")))

  (pp/pprint
   (l/lex
    "int main(void) {return (((2)))}"))

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
