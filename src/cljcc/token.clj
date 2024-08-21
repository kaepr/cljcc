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
    :multiply
    :divide
    :remainder
    :plus
    :minus
    :logical-not
    :logical-and
    :logical-or
    :equal-to
    :not-equal-to
    :less-than
    :greater-than
    :less-than-equal-to
    :greater-than-equal-to
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

(def unary-ops
  #{:logical-not
    :complement
    :hyphen})

(defn unary-op? [op]
  (contains? unary-ops op))

(def bin-ops
  "Binary operands and their precedence."
  {:multiply 100
   :divide 100
   :remainder 100

   :plus 90
   :hyphen 90

   :bitwise-left-shift 80
   :bitwise-right-shift 80

   :less-than 70
   :less-than-equal-to 70
   :greater-than 70
   :greater-than-equal-to 70

   :equal-to 60
   :not-equal-to 60

   :ampersand 50

   :bitwise-xor 40

   :bitwise-or 30

   :logical-and 20

   :logical-or 10

   :assignment 1})

(defn binary-op? [op]
  (contains? bin-ops op))

(defn precedence [op]
  (op bin-ops))

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
   \! :logical-not
   "&&" :logical-and
   "||" :logical-or
   "==" :equal-to
   "!=" :not-equal-to
   \< :less-than
   \> :greater-than
   "<=" :less-than-equal-to
   ">=" :greater-than-equal-to
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
