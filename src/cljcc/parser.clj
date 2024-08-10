(ns cljcc.parser
  (:require
   [instaparse.core :as insta]
   [clojure.pprint :as pp]))

(def whitespace
  (insta/parser
   "whitespace = #'\\s+'"))

(declare parse)

(def c-parser
  (insta/parser
   "<program> = function+
    function = #'int\\b' identifier <'('> #'void\\b' <')'> <'{'> statement <'}'>
    statement = #'return\\b' exp <';'>
    exp = exp-prime
    <exp-prime> = <'('> exp-prime <')'> | unop-exp | constant-exp
    unop-exp = unop exp
    unop = #'-' | #'~'
    identifier = #'[a-zA-Z_]\\w*\\b'
    constant-exp = #'[0-9]+\\b'
    keyword = #'int\\b' | #'return\\b' | #'void\\b'"
   :auto-whitespace whitespace))

(def binop-parser
  (insta/parser
   "<program> = function+
    function = #'int\\b' identifier <'('> #'void\\b' <')'> <'{'> statement <'}'>
    statement = #'return\\b' exp <';'>
    exp = exp-prime
    <exp-prime> = mul-div-mod | add-exp | sub-exp
    add-exp = exp-prime <'+'> mul-div-mod
    sub-exp = exp-prime <'-'> mul-div-mod
    <mul-div-mod> = term | mul-exp | div-exp | mod-exp
    mul-exp = mul-div-mod <'*'> term
    div-exp = mul-div-mod <'/'> term
    mod-exp = mul-div-mod <'%'> term
    <term> = constant-exp | unary-exp | <'('> exp-prime <')'>
    unary-exp = unary-operator term
    unary-operator = #'-' | #'~'
    identifier = #'[a-zA-Z_]\\w*\\b'
    constant-exp = #'[0-9]+\\b'
    keyword = #'int\\b' | #'return\\b' | #'void\\b'"
   :auto-whitespace whitespace))

(defn parseable? [result]
  (not (insta/failure? result)))

(defn parse [source]
  (binop-parser source))

(comment

  (parse "int main(void) {return 2;}")

  (parse "
  int main(void) {
  return 2;
  }")

  (parse "int main(void) {
   return -(((((10)))));
   }")

  (pp/pprint (parse "int main(void) {
   return -(((((10)))));
   }"))

  (pp/pprint
   (binop-parser
    "int main(void) {
    return -1 * 2 - ~3 * -(-4 + 5);
     }"))

  (pp/pprint
   (binop-parser
    "int main(void) {
       return -2;
     }"))

  ())
