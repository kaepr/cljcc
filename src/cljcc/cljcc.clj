(ns cljcc.cljcc
  (:require
   [instaparse.core :as insta]
   [clojure.java.io :as io])
  (:gen-class))

(def ex-prg "int main(void) {return 2;}")

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

(println (c-parser ex-prg))

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [input-file-path (first args)])
  (greet {:name (first args)}))
