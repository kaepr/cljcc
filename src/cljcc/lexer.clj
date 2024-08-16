(ns cljcc.lexer
  (:require
   [cljcc.util :refer [newline? whitespace? read-number digit? letter-digit? letter?]]
   [cljcc.token :as t]
   [clojure.pprint :as pp]))

(defn- lexer-ctx []
  {:tokens []
   :line 1
   :col 1})

(defn lex
  ([source]
   (lex source 0 (lexer-ctx)))
  ([[ch :as source] pos {:keys [line col] :as ctx}]
   (cond
     (empty? source) (update ctx :tokens #(conj % (t/create :eof line col)))
     (newline? ch) (recur (next source)
                          (+ pos 1)
                          (-> ctx
                              (update :line inc)
                              (update :col (fn [_] 1))))
     (contains?
      t/chrs-kind-map ch) (recur (next source)
                                 (+ pos 1)
                                 (-> ctx
                                     (update :col inc)
                                     (update :tokens #(conj % (t/create (get t/chrs-kind-map ch) line col)))))
     (whitespace? ch) (recur (next source)
                             (+ pos 1)
                             (-> ctx
                                 (update :col inc)))
     (digit? ch) (let [[chrs rst] (split-with letter-digit? source)
                       number (read-number (apply str chrs))
                       cnt (count chrs)
                       npos (+ pos cnt)
                       token (t/create :number line col number)]
                   (recur (apply str rst)
                          npos
                          (-> ctx
                              (update :col #(+ % cnt))
                              (update :tokens #(conj % token)))))
     (letter? ch) (let [[chrs rst] (split-with letter-digit? source)
                        lexeme (apply str chrs)
                        cnt (count chrs)
                        kind (t/identifier->kind lexeme)
                        token (if (= :identifier kind)
                                (t/create kind line col lexeme)
                                (t/create kind line col))
                        npos (+ pos cnt)]
                    (recur (apply str rst) npos (-> ctx
                                                    (update :col #(+ % cnt))
                                                    (update :tokens #(conj % token)))))
     :else (throw (ex-info "Lexer error. Invalid token." {:line line :col col})))))

(comment

  "int main(void) {
    return 2;
  }"

  (pp/pprint
   (lex "int main(void) {return 2;}"))

  ())
