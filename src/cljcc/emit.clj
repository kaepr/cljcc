(ns cljcc.emit
  (:require [cljcc.parser :as p]
            [cljcc.util :refer [get-os]]
            [clojure.string :as str]))

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

(defn emit-function-assembly [fn-ast]
  (let [name (handle-function-name (:identifier fn-ast))
        globl-line (str "    .globl " name)
        fn-start-line (str name ":")
        body-statements (map statement-fn (:body fn-ast))]
    (flatten [globl-line fn-start-line body-statements])))

(def linux-assembly-end ".section .note.GNU-stack,\"\",@progbits")

(defn emit-assembly [ast]
  (let [fn-assembly (emit-function-assembly (first ast))]
    (if (= :linux (get-os))
      (concat fn-assembly [linux-assembly-end])
      fn-assembly)))

(defn join-assembly [assembly-lines]
  (str/join "\n" assembly-lines))

(comment

  (def ex "int main(void) {return 2;}")

  (-> ex
      p/parse)

  ())
