(ns cli.driver
  (:require [cljcc.cljcc :as cljcc]
            [cli.core.log :as log]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.java.shell :refer [sh]]
            [clojure.java.io :as io]))

(defn- remove-extension [^String filename]
  (if (.contains filename ".")
    (.substring filename 0 (.lastIndexOf filename "."))
    filename))

(defn- get-os []
  (let [os-name (.toLowerCase (System/getProperty "os.name"))]
    (cond
      (.contains os-name "mac") :mac
      (.contains os-name "linux") :linux
      :else :unsupported)))

(defn mac-aarch64? []
  (and (= :mac (get-os)) (= (System/getProperty "os.arch") "aarch64")))

(defn- validate-os []
  (let [os (get-os)]
    (condp = os
      :linux (log/info "Running on Linux.")
      :mac (if (mac-aarch64?)
             (log/info "Running on Mac ARM64.")
             (log/info "Running on Mac x86_64."))
      :unsupported (throw (Exception. (str os " is not currently supported."))))))

(defn- make-file-name
  ([^String filename ^String ext]
   (str filename "." ext))
  ([directory filename ext]
   (str directory "/" filename "." ext)))

(defn- handle-sh
  "Preprends arch -x86_64 if running under Mac M chips."
  [command & args]
  (let [args (filterv (comp not empty?) args)]
    (if (mac-aarch64?)
      (apply sh "arch" "-x86_64" command args)
      (apply sh command args))))

(defn- cleanup-step [directory filename]
  (let [file-without-ext (remove-extension filename)]
    (io/delete-file (make-file-name directory file-without-ext "i") true)
    (io/delete-file (make-file-name directory file-without-ext "s") true)))

(defn- preprocessor-step [directory filename]
  (let [input-file-path (make-file-name directory (remove-extension filename) "c")
        preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        output (handle-sh "gcc" "-E" "-P" input-file-path "-o" preprocessed-file-path)]
    (if (= 1 (:exit output))
      (throw (Exception. ^String (:err output)))
      (log/info (str "Successfully preprocessed file: " preprocessed-file-path)))))

(defn- lexer-step [directory filename]
  (let [preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)
        output (cljcc/run source :options {:target {:os (get-os)}
                                           :stage :lex})]
    (log/info "Input file is succesfully lexed.")
    (pp/pprint output)))

(defn- parser-step [directory filename]
  (let [preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)
        parsed-ast (cljcc/run source :options {:target {:os (get-os)}
                                               :stage :parse})]
    (log/info "Input file is succesfully parsed.")
    (pp/pprint parsed-ast)))

(defn- semantic-analyzer-step [directory filename]
  (let [preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)
        validated-ast (cljcc/run source :options {:target {:os (get-os)}
                                                  :stage :validate})]
    (log/info "Input file is succesfully validated.")
    (pp/pprint validated-ast)))

(defn- tacky-step [directory filename]
  (let [preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)
        output (cljcc/run source :options {:target {:os (get-os)}
                                           :stage :tacky})]
    (log/info (str
               "Successfully generated Tacky IR.\n"
               (with-out-str (pp/pprint output))))))

(defn- compiler-step [directory filename]
  (let [preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)
        output (cljcc/run source :options {:target {:os (get-os)}
                                           :stage :codegen})]
    (log/info (str "Succesfully generated assembly ast.\n" output))))

(defn- assemble-step [directory filename options]
  (let [file-without-ext (remove-extension filename)
        assembly-file (make-file-name directory file-without-ext "s")
        preprocessed-file-path (make-file-name directory (remove-extension filename) "i")
        file (io/file preprocessed-file-path)
        source (slurp file)
        assembly-output (cljcc/run source :options {:target {:os (get-os)}
                                                    :stage :emit})
        assembly-out-file-path (make-file-name directory (remove-extension filename) "s")
        _ (spit assembly-out-file-path assembly-output)
        output-file (if (:generate-object-file options)
                      (str directory "/" (str file-without-ext ".o"))
                      (str directory "/" file-without-ext))
        libs (str/join " " (:libs options))
        output (if (:generate-object-file options)
                 (handle-sh "gcc" "-c" assembly-file "-o" output-file libs)
                 (handle-sh "gcc" assembly-file "-o" output-file libs))]
    (if (= 1 (:exit output))
      (throw (Exception. ^String (:err output)))
      (log/info (str "Successfully created executable at: " output-file)))))

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

(defn run [^String file-path options]
  (let [file (io/file ^String file-path)
        filename (.getName file)
        directory (.getParent file)]
    (try
      (run-steps options directory filename)
      (finally
        (cleanup-step directory filename)))))
