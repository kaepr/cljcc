(ns cljcc.driver
  (:require
   [clojure.java.io :as io]
   [cljcc.compiler :as c]
   [cljcc.tacky :as t]
   [cljcc.lexer :as l]
   [cljcc.emit :as e]
   [cljcc.analyzer :as a]
   [clojure.pprint :as pp]
   [cljcc.log :as log]
   [cljcc.util :refer [get-os handle-sh mac-aarch64? make-file-name]]
   [cljcc.parser :as p]
   [clojure.string :as str]))

(defn- validate-os []
  (let [os (get-os)]
    (condp = os
      :linux (log/info "Running on Linux.")
      :mac (if (mac-aarch64?)
             (log/info "Running on Mac ARM64.")
             (log/info "Running on Mac x86_64."))
      :unsupported (throw (Exception. (str os " is not currently supported."))))))

(defn- remove-extension [^String filename]
  (if (.contains filename ".")
    (.substring filename 0 (.lastIndexOf filename "."))
    filename))

(defn- preprocessor-step [directory filename]
  (let [input-file-path (make-file-name directory (remove-extension filename) "c")
        preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        output (handle-sh "gcc" "-E" "-P" input-file-path "-o" preprocessed-file-path)]
    (if (= 1 (:exit output))
      (throw (Exception. ^String (:err output)))
      (log/info (str "Successfully preprocessed file: " preprocessed-file-path)))))

(defn- assemble-step [directory filename options]
  (let [file-without-ext (remove-extension filename)
        assembly-file (make-file-name directory file-without-ext "s")
        preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)
        assembly-ast (c/generate-assembly source)
        assembly-output (e/emit assembly-ast)
        assembly-out-file-path (make-file-name directory (remove-extension filename) "s")
        _ (println assembly-output)
        _ (spit assembly-out-file-path assembly-output)
        output-file (if (:generate-object-file options)
                      (str directory "/" (str file-without-ext ".o"))
                      (str directory "/" file-without-ext))
        output (if (:generate-object-file options)
                 (handle-sh "gcc" "-c" assembly-file "-o" output-file)
                 (handle-sh "gcc" assembly-file "-o" output-file))]
    (if (= 1 (:exit output))
      (throw (Exception. ^String (:err output)))
      (log/info (str "Successfully created executable at: " output-file)))))

(defn- parser-step [directory filename]
  (let [preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)
        ast (p/parse (l/lex source))]
    (log/info "Input file is succesfully parsed.")
    (pp/pprint ast)))

(defn- semantic-analyzer-step [directory filename]
  (let [preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)
        ast (a/validate (p/parse (l/lex source)))]
    (log/info "Input file is succesfully validated.")
    (pp/pprint ast)))

(defn- lexer-step [directory filename]
  (let [preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)
        output (l/lex source)]
    (log/info "Input file is succesfully lexed.")
    (pp/pprint output)))

(defn- tacky-step [directory filename]
  (let [preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)
        output (t/tacky-generate (a/validate (p/parse (l/lex source))))]
    (log/info (str
               "Successfully generated Tacky IR.\n"
               (with-out-str (pp/pprint output))))))

(defn- compiler-step [directory filename]
  (let [preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)
        assembly-ast (c/generate-assembly source)]
    (log/info (str "Succesfully generated assembly ast.\n" assembly-ast))))

(defn- cleanup-step [directory filename]
  (let [file-without-ext (remove-extension filename)]
    (io/delete-file (make-file-name directory file-without-ext "i") true)
    (io/delete-file (make-file-name directory file-without-ext "s") true)))

(defn- create-steps [options directory filename]
  (let [steps [(partial validate-os)
               (partial preprocessor-step directory filename)
               (partial lexer-step directory filename)
               (partial parser-step directory filename)
               (partial semantic-analyzer-step directory filename)
               (partial tacky-step directory filename)
               (partial compiler-step directory filename)
               (partial assemble-step directory filename options)]]
    (cond
      (:lex options) (subvec steps 0 3)
      (:parse options) (subvec steps 0 4)
      (:validate options) (subvec steps 0 5)
      (:tacky options) (subvec steps 0 6)
      (:codegen options) (subvec steps 0 7)
      :else steps)))

(defn- run-steps [options directory filename]
  (let [steps (create-steps options directory filename)]
    (run! #(apply % []) steps)))

(defn run
  "Runs the compiler driver with the given input source file."
  [^String file-path options]
  (let [file (io/file ^String file-path)
        filename (.getName file)
        directory (.getParent file)]
    (try
      (run-steps options directory filename)
      (finally
        (cleanup-step directory filename)))))

(comment

  (run "./test-programs/ex1.c" {}))
