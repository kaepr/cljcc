(ns cljcc.util
  (:require [clojure.java.shell :refer [sh]]
            [cljcc.log :as log]))

(defn make-file-name
  ([^String filename ^String ext]
   (str filename "." ext))
  ([directory filename ext]
   (str directory "/" filename "." ext)))

(defn get-os []
  (let [os-name (.toLowerCase (System/getProperty "os.name"))]
    (cond
      (.contains os-name "mac") :mac
      (.contains os-name "linux") :linux
      :else :unsupported)))

(defn mac-aarch64? []
  (and (= :mac (get-os)) (= (System/getProperty "os.arch") "aarch64")))

(defn handle-sh
  "Preprends arch -x86_64 if running under Mac M chips."
  [command & args]
  (if (mac-aarch64?)
    (apply sh "arch" "-x86_64" command args)
    (apply sh command args)))

(defn exit
  ([status msg]
   (if (= status 0)
     (log/info msg)
     (log/error msg))
   (System/exit status))
  ([status msg e]
   (log/error (ex-data e))
   (exit status msg)))

(defn letter? [^Character ch]
  (or (= \_ ch)
      (Character/isLetter ch)))

(defn letter-digit? [^Character ch]
  (or (= \_ ch)
      (Character/isLetterOrDigit ch)))

(defn digit? [^Character ch]
  (Character/isDigit ch))

(defn newline? [ch]
  (= \newline ch))

(defn whitespace? [^Character ch]
  (Character/isWhitespace ch))

(defn read-number [str]
  (try
    (Integer/parseInt str)
    (catch Exception e
      (throw (ex-info "Lexer error. Malformed number." {:message (.getMessage e)})))))
