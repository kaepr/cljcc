(ns cljcc.compiler
  (:require [cljcc.parser :as p]
            [instaparse.core :as insta]
            [clojure.edn :as edn]
            [cljcc.util :refer [get-os]]
            [clojure.string :as str]))

(defn transform-function [return-type identifier args body]
  {:op :function
   :identifier identifier
   :args args
   :body body})

(defn ast->compile [ast]
 (insta/transform
  {:function transform-function
   :identifier str
   :constant (comp edn/read-string str)
   :exp (fn [v]
          {:op :movl
           :src v
           :dst :eax})
    :statement (fn [_ v]
                 [v {:op :ret}])}
  ast))

(defn handle-function-name [name]
  (if (= :mac (get-os))
    (str "_" name)
    name))

(defn emit-instruction
  ([inst]
   (str "    " (symbol inst)))
  ([inst src dst]
   (str "    " (symbol inst) "    " "$" src ", %" (symbol dst))))

(defn statement-fn [stmt]
  (condp = (:op stmt)
        :ret (emit-instruction :ret)
        :movl (emit-instruction (:op stmt) (:src stmt) (:dst stmt))))

(statement-fn {:op :movl :src 1 :dst :eax})

(defn emit-function-assembly [fn-ast]
  (let [name (handle-function-name (:identifier fn-ast))
        globl-line (str "    .globl " name)
        fn-start-line (str name ":")
        body-statements (map statement-fn (:body fn-ast))]
    (flatten [globl-line fn-start-line body-statements])))

(def linux-assembly-end ".section .note.GNU-stack,\"\",@progbits")

(defn il->assembly [il]
  (let [fn-assembly (emit-function-assembly (first il))]
    (if (= :linux (get-os))
      (conj fn-assembly linux-assembly-end)
      fn-assembly)))

(defn join-assembly [assembly-lines]
  (str/join "\n" assembly-lines))

(defn run-compile [source]
  (-> source
      p/parse
      ast->compile
      il->assembly
      join-assembly))

(comment

 (def ex "int main(void) {return 2;}")

 (-> ex
      p/parse)

 (-> ex
      p/parse
      ast->compile)

 (-> ex
   p/parse
   ast->compile
   il->assembly
   join-assembly)

 ,)
