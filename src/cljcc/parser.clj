(ns cljcc.parser
  (:require
   [cljcc.lexer :as l]
   [clojure.pprint :as pp]))

(defn- expect [expected-kind [token & rst]]
  (if (= expected-kind (:kind token))
    [token rst]
    (throw (ex-info "Parser Error." {:expected expected-kind
                                     :actual (:kind token)}))))

(defn- parse-exp [tokens]
  (let [[t rst] (expect :number tokens)]
    [{:type :exp
      :value {:type :constant-exp
              :value (:literal t)}} rst]))

(defn- parse-return-statement [tokens]
  (let [[_ rst] (expect :kw-return tokens)
        [constant-node rst] (parse-exp rst)]
    [{:type :statement
      :statement-type :return
      :value constant-node}
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

  (parse "int main(void) {return 2;}")

  (pp/pprint (parse (l/lex "
  int main(void) {
  return 2;
  }")))

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
