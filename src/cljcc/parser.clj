(ns cljcc.parser
  (:require
   [instaparse.core :as insta]))

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
    <exp-prime> = constant | unop-exp | <'('> exp-prime <')'>
    unop-exp = unop exp
    unop = #'-' | #'~'
    identifier = #'[a-zA-Z_]\\w*\\b'
    constant = #'[0-9]+\\b'
    keyword = #'int\\b' | #'return\\b' | #'void\\b'"
   :auto-whitespace whitespace))

(defn parseable? [result]
  (not (insta/failure? result)))

(defn parse [source]
  (c-parser source))

(comment

  (parse "int main(void) {return 2;}")

  (parse "
  int main(void) {
  return 2;
  }")

  (parse "int main(void) {
   return -(((((10)))));
   }")

  (parse "int main(void) {
   return -(((((10)))));
   }"))
