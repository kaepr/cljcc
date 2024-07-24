(ns cljcc.driver
  (:require [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]
            [cljcc.compiler :as c]
            [cljcc.util :refer [get-os handle-sh mac-aarch64?]]))

(defn make-file-name
  ([filename ext]
   (str filename "." ext))
  ([directory filename ext]
   (str directory "/" filename "." ext)))

(defn handle-os []
  (let [os (get-os)]
    (condp = os
      :linux (println "running on linux")
      :mac (if (mac-aarch64?)
             (println "running on mac arch 64")
             (println "running on mac"))
      :unsupported (throw (Exception. (str os " is not currently supported."))))))

(defn remove-extension [^String filename]
  (if (.contains filename ".")
    (.substring filename 0 (.lastIndexOf filename "."))
    filename))

(defn preprocess [directory filename]
  (let [input-file-path (make-file-name directory (remove-extension filename) "c")
        preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        output (handle-sh "gcc" "-E" "-P" input-file-path "-o" preprocessed-file-path)]
    (if (= 1 (:exit output))
      (throw (Exception. ^String (:out output)))
      (println (str "Successfully preprocessed file: " preprocessed-file-path)))))

(defn assemble [directory filename]
  (let [file-without-ext (remove-extension filename)
        assembly-file (make-file-name directory file-without-ext "s")
        output-file (str directory "/" file-without-ext)
        output (handle-sh "gcc" assembly-file "-o" output-file)]
    (println file-without-ext assembly-file output-file output)
    (if (= 1 (:exit output))
      (throw (Exception. ^String (:out output)))
      (println (str "Successfully created executable at: " output-file output)))))

(defn run-compile [directory filename]
  (let [preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)
        assembled-source (c/run-compile source)
        out-file-path (make-file-name directory (remove-extension filename) "s")]
    (spit out-file-path assembled-source)
    (println "succesfully generated .s file" assembled-source)))

(defn cleanup [directory filename]
  (let [file-without-ext (remove-extension filename)]
    (sh "rm" (make-file-name directory file-without-ext "i"))
    (sh "rm" (make-file-name directory file-without-ext "s"))))

(defn run
  "Runs the compiler driver with the given input source file."
  [^String file-path]
  (let [file (io/file ^String file-path)
        _ (println file)
        filename (.getName file)
        directory (.getParent file)]
    (handle-os)
    (preprocess directory filename)
    (run-compile directory filename)
    (assemble directory filename)
    (cleanup directory filename)
    (println "Successfully created executable at " directory " for filename " filename)))

(comment

 (run "/Users/shagunagrawal/Development/c_tests/ex2.c")

 (assemble "/Users/shagunagrawal/Development/c_tests" "ex2.c")

 (handle-sh "gcc" "-E" "-P" "/Users/shagunagrawal/Development/c_tests/ex2.c" "-o" "/Users/shagunagrawal/Development/c_tests/out.i")

 (sh "gcc" "-E" "-P" "/Users/shagunagrawal/Development/c_tests/ex1.c" "-o" "/Users/shagunagrawal/Development/c_tests/out.i")

 ,)
