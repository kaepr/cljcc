(ns cljcc.driver
  (:require
        [clojure.java.io :as io]
        [cljcc.compiler :as c]
        [cljcc.log :as log]
        [cljcc.util :refer [get-os handle-sh mac-aarch64? make-file-name]]
        [cljcc.parser :as p]))

(defn validate-os []
  (let [os (get-os)]
    (condp = os
      :linux (log/info "Running on Linux.")
      :mac (if (mac-aarch64?)
             (log/info "Running on Mac ARM64.")
             (log/info "Running on Mac x86_64."))
      :unsupported (throw (Exception. (str os " is not currently supported."))))))

(defn remove-extension [^String filename]
  (if (.contains filename ".")
    (.substring filename 0 (.lastIndexOf filename "."))
    filename))

(defn preprocessor-step [directory filename]
  (let [input-file-path (make-file-name directory (remove-extension filename) "c")
        preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        output (handle-sh "gcc" "-E" "-P" input-file-path "-o" preprocessed-file-path)]
    (if (= 1 (:exit output))
      (throw (Exception. ^String (:err output)))
      (log/info (str "Successfully preprocessed file: " preprocessed-file-path)))))

(defn assemble-step [directory filename]
  (let [file-without-ext (remove-extension filename)
        assembly-file (make-file-name directory file-without-ext "s")
        output-file (str directory "/" file-without-ext)
        output (handle-sh "gcc" assembly-file "-o" output-file)]
    (if (= 1 (:exit output))
      (throw (Exception. ^String (:err output)))
      (log/info (str "Successfully created executable at: " output-file)))))

(defn parser-step [directory filename]
  (let [preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)]
    (if (p/parseable? (p/parse source))
      (log/info "Input file is succesfully parsed.")
      (throw (Exception. "Failed during parsing")))))

(defn compiler-step [directory filename]
  (let [preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)
        assembled-source (c/run-compile source)
        out-file-path (make-file-name directory (remove-extension filename) "s")]
    (spit out-file-path assembled-source)
    (log/info (str "Succesfully generated assembly file.\n" assembled-source))))

(defn cleanup-step [directory filename]
  (let [file-without-ext (remove-extension filename)]
    (io/delete-file (make-file-name directory file-without-ext "i") true)
    (io/delete-file (make-file-name directory file-without-ext "s") true)))

(defn create-steps [options directory filename]
  (let [base-steps [(partial validate-os)
                    (partial preprocessor-step directory filename)]
        parser-step-fn (partial parser-step directory filename)
        compiler-step-fn (partial compiler-step directory filename)
        assemble-step-fn (partial assemble-step directory filename)
        cleanup-step-fn (partial cleanup-step directory filename)]
    (cond
      (:parse options) (concat base-steps
                               [parser-step-fn cleanup-step-fn])
      (:codegen options) (concat base-steps
                                 [parser-step-fn compiler-step-fn cleanup-step-fn])
      :else (concat base-steps
                    [parser-step-fn compiler-step-fn assemble-step-fn cleanup-step-fn]))))

(defn run-steps [options directory filename]
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

 (run "./test-programs/ex1.c" {})

 ,)
