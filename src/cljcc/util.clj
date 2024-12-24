(ns cljcc.util
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [cljcc.log :as log]
            [cljcc.exception :as exc]))

(def ^:private counter "Global integer counter for generating unique identifier names." (atom 0))

(set! *warn-on-reflection* true)

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
  (let [args (filterv (comp not empty?) args)]
    (if (mac-aarch64?)
      (apply sh "arch" "-x86_64" command args)
      (apply sh command args))))

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

(defn matches-regex [re s]
  (not (nil? (re-matches re s))))

(def unsigned-long-re #"[0-9]+([lL][uU]|[uU][lL])")
(def signed-long-re #"[0-9]+[lL]")
(def unsigned-int-re #"[0-9]+[uU]")
(def signed-int-re #"[0-9]+")
(def fractional-constant #"([0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\.")

(defn read-number
  "Returns number in string form.

  Checks whether number is valid long. If no, checks if it valid int.
  Otherwise error."
  [s line col]
  (if-let [_ (or (matches-regex signed-int-re s)
                 (matches-regex signed-long-re s)
                 (matches-regex unsigned-int-re s)
                 (matches-regex unsigned-long-re s))]
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

(defn get-type-size [t]
  (condp = t
    {:type :int} 5
    {:type :uint} 5
    {:type :long} 10
    {:type :ulong} 10
    (exc/analyzer-error "Invalid type passed to get-type-size." {:type t})))

(defn type-signed? [t]
  (condp = t
    {:type :int} true
    {:type :long} true
    {:type :uint} false
    {:type :ulong} false
    (exc/analyzer-error "Invalid type passed to type-signed?." {:type t})))
