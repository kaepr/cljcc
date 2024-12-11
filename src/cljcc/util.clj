(ns cljcc.util
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [cljcc.log :as log]
            [cljcc.exception :as exc]))

(def ^:private counter "Global integer counter for generating unique identifier names." (atom 0))

(defn create-identifier!
  "Returns a unique identifier. Used for generating unique identifier.

  Removes : from keywords.
  Replaces all - with _ for generating valid assembly names."
  ([]
   (create-identifier! "tmp"))
  ([identifier]
   (let [n @counter
         _ (swap! counter inc)]
     (-> identifier
         (str "." n)
         (str/replace #":" "")
         (str/replace #"-" "_")))))

(defn reset-counter! []
  (reset! counter 0))

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

(defn- valid-long?
  "Validates string to be of form [0-9]+[lL]\b.

  Verifies that `l` or `L` occurs only once, and at the end."
  [s]
  (try
    (let [strip-l-or-L (if-let [_ (or (str/ends-with? s "l")
                                      (str/ends-with? s "L"))]
                         (subs s 0 (dec (count s)))
                         s)
          _ (-> strip-l-or-L
                Long/parseLong
                Long/toString)]
      s)
    (catch Exception _e
      false)))

(defn read-number
  "Returns number and number type tuple.

  Checks whether number is valid long. If no, checks if it valid int.
  Otherwise error."
  [s line col]
  (if-let [s (valid-long? s)]
    s
    (exc/lex-error {:line line
                    :col col})))

(defn round-away-from-zero [num div]
  (let [div (abs div)]
    (cond
      (= (mod num div) 0) num
      (< num 0) (- num (- div (mod num div)))
      :else (+ num (- div (mod num div))))))

(defn in-int-range?
  "Verifies whether -2^31 <= x <= 2^31."
  [v]
  (and (>= v Integer/MIN_VALUE)
       (<= v Integer/MAX_VALUE)))

(not (in-int-range? Long/MAX_VALUE))
