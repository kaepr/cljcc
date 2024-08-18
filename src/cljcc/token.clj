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
    :bitwise-not

    :multiply
    :divide
    :remainder

    :plus
    :minus

    :bitwise-left-shift
    :bitwise-right-shift

    :ampersand

    :bitwise-xor

    :bitwise-or

    :negate
    :assignemnt
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

(def bin-ops
  "Binary operanrs and their precedence."
  {:multiply 100
   :divide 100
   :remainder 100
   :plus 90
   :hyphen 90
   :bitwise-left-shift 80
   :bitwise-right-shift 80
   :ampersand 70
   :bitwise-xor 60
   :bitwise-or 50})

(def chrs-kind-map
  {\( :left-paren
   \) :right-paren
   \{ :left-curly
   \} :right-curly
   \= :assignment
   "--" :decrement
   "++" :increment
   "<<" :bitwise-left-shift
   ">>" :bitwise-right-shift
   \^ :bitwise-xor
   \| :bitwise-or
   \& :ampersand
   \; :semicolon
   \+ :plus
   \- :hyphen
   \~ :complement
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
