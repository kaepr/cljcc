(ns cljcc.core.exception
  (:require [cljcc.core.format :refer [safe-format]]))

(defn try-catch-ex
  ([f]
   (try
     (f)
     (catch #?(:clj Throwable :cljs :default) e
       [:error e])))
  ([f default]
   (try
     (f)
     (catch #?(:clj Throwable :cljs :default) e
       default))))

(defn lex-error [{line :line col :col :as data}]
  (throw (ex-info
          (safe-format "Invalid token at line: %s, col: %s." line col)
          (merge {:error/type :lexer} data))))

(defn parser-error [msg data]
  (throw (ex-info msg (merge {:error/type :parser} data))))

(defn analyzer-error [msg data]
  (throw (ex-info msg (merge {:error/type :analyzer} data))))

(defn tacky-error [msg data]
  (throw (ex-info msg (merge {:error/type :tacky} data))))

(defn compiler-error [msg data]
  (throw (ex-info msg (merge {:error/type :compiler} data))))

(defn emit-error [msg data]
  (throw (ex-info msg (merge {:error/type :emit} data))))
