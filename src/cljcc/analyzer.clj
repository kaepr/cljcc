(ns cljcc.analyzer
  (:require [cljcc.lexer :as l]
            [cljcc.parser :as p]))

()

(defn validate [ast])

(defn- validate-from-src [s]
  (-> s
      l/lex
      p/parse
      validate))

(comment

  ())
