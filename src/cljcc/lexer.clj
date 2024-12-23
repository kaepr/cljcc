(ns cljcc.lexer
  (:require
   [cljcc.util :refer [newline? whitespace? read-number digit? letter-digit? letter?]]
   [cljcc.exception :as exc]
   [cljcc.token :as t]))

(defn- lexer-ctx []
  {:tokens []
   :line 1
   :col 1})

(defn lex
  ([source]
   (lex source (lexer-ctx)))
  ([[ch pk th :as source] {:keys [line col] :as ctx}]
   (cond
     (empty? source) (update ctx :tokens #(conj % (t/create :eof line col)))
     (newline? ch) (recur (next source)
                          (-> ctx
                              (update :line inc)
                              (update :col (fn [_] 1))))
     (contains?
      t/chrs-kind-map (str ch pk th)) (recur (next (next (next source)))
                                             (-> ctx
                                                 (update :col #(+ % 3))
                                                 (update :tokens #(conj % (t/create (get t/chrs-kind-map (str ch pk th)) line col)))))
     (contains?
      t/chrs-kind-map (str ch pk)) (recur (next (next source))
                                          (-> ctx
                                              (update :col #(+ % 2))
                                              (update :tokens #(conj % (t/create (get t/chrs-kind-map (str ch pk)) line col)))))
     (contains?
      t/chrs-kind-map ch) (recur (next source)
                                 (-> ctx
                                     (update :col inc)
                                     (update :tokens #(conj % (t/create (get t/chrs-kind-map ch) line col)))))
     (whitespace? ch) (recur (next source)
                             (-> ctx
                                 (update :col inc)))
     (digit? ch) (let [[chrs rst] (split-with letter-digit? source)
                       number (read-number (apply str chrs) line col)
                       cnt (count chrs)
                       token (t/create :number line col number)]
                   (recur (apply str rst)
                          (-> ctx
                              (update :col #(+ % cnt))
                              (update :tokens #(conj % token)))))
     (letter? ch) (let [[chrs rst] (split-with letter-digit? source)
                        lexeme (apply str chrs)
                        cnt (count chrs)
                        kind (t/identifier->kind lexeme)
                        token (if (= :identifier kind)
                                (t/create kind line col lexeme)
                                (t/create kind line col))]
                    (recur (apply str rst) (-> ctx
                                               (update :col #(+ % cnt))
                                               (update :tokens #(conj % token)))))
     :else (exc/lex-error {:line line :col col}))))

(comment

  (lex
   "
int main(void) {
    return 42;
}
")

  ())
