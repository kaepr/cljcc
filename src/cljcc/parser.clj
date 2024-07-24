(ns cljcc.parser
  (:require
   [instaparse.core :as insta]))

(def whitespace
  (insta/parser
   "whitespace = #'\\s+'"))

(def c-parser
  (insta/parser
   "<program> = function+
    function = #'int\\b' identifier <'('> #'void\\b' <')'> <'{'> statement <'}'>
    statement = #'return\\b' exp <';'>
    exp = constant
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
 ,)
