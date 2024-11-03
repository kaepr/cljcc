(ns cljcc.exception)

(defn lex-error [{line :line col :col :as data}]
  (throw (ex-info
          (format "Invalid token at line: %s, col: %s." line col)
          (merge {:error/type :lexer} data))))

(defn parser-error [msg data]
  (throw (ex-info msg (merge {:error/type :parser} data))))
