(ns cljcc.exception)

(defn lex-error [{line :line col :col msg :msg}]
  (let [err-msg (if (empty? msg)
                  (format "Lexer error. Invalid token at line: %s, col: %s." line col)
                  (format "Lexer error. Invalid token at line: %s, col: %s. %s" line col msg))]
    (throw (ex-info err-msg {}))))
