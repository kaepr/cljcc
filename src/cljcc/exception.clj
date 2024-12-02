(ns cljcc.exception)

(defn lex-error [{line :line col :col :as data}]
  (throw (ex-info
          (format "Invalid token at line: %s, col: %s." line col)
          (merge {:error/type :lexer} data))))

(defn parser-error [msg data]
  (throw (ex-info msg (merge {:error/type :parser} data))))

(defn analyzer-error [msg data]
  (throw (ex-info msg (merge {:error/type :analyzer} data))))

(defn tacky-error [msg data]
  (throw (ex-info msg (merge {:error/type :tacky} data))))

(defn compiler-error [msg data]
  (throw (ex-info msg (merge {:error/type :compiler} data))))
