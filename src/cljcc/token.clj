(ns cljcc.token)

(def token-kind
  #{:eof
    :semicolon

    ;; brackets
    :left-curly
    :right-curly
    :left-paren
    :right-paren

    ;; operators
    :plus
    :minus
    :multiply
    :divide
    :remainder
    :negate
    :assignemnt
    :ampersand
    :bitwise-not
    :bitwise-or
    :bitwise-xor
    :bitwise-left
    :increment
    :decrement

    :number
    :identifier

    ;; keywords
    :kw-return
    :kw-int
    :kw-void})

(def chrs
  #{})

(def chrs-kind-map
  {\( :left-paren
   \) :right-paren
   \{ :left-curly
   \} :right-curly
   \= :assignment
   "--" :decrement
   "++" :increment
   \; :semicolon
   \+ :plus
   \- :minus
   \* :multiply
   \% :remainder
   \/ :divide})

(defn identifier->kind [identifier]
  (case identifier
    "return" :kw-return
    "void" :kw-void
    "int" :kw-int
    :identifier))

(defn create
  ([kind line col]
   {:kind kind
    :line line
    :col col})
  ([kind line col literal]
   {:kind kind
    :line line
    :col col
    :literal literal}))
