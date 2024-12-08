(ns cljcc.symbol)

;; Contains functions related to symbol table manipulation.

(defn create-symbol [type attribute]
  {:type type
   :attribute attribute})

(defn local-attribute []
  {:type :local})

(defn static-attribute [initial-value global?]
  {:type :static
   :initial-value initial-value
   :global? global?})

(defn fun-attribute [defined? global?]
  {:type :fun
   :defined? defined?
   :global? global?})

(defn no-initializer-iv []
  {:type :no-initializer})

(defn tentative-iv []
  {:type :tentative})

(defn initial-iv [static-init]
  {:type :initial
   :static-init static-init})

(defn int-init [v]
  {:type :int-init
   :value v})

(defn long-init [v]
  {:type :long-init
   :value v})
