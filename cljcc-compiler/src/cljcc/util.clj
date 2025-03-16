(ns cljcc.util
  (:require
   [clojure.string :as str]
   [cljcc.core.exception :as exc]))

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

(defn letter? [^Character ch]
  (or (= \_ ch)
      (Character/isLetter ch)))

(defn letter-digit? [^Character ch]
  (or (= \_ ch)
      (Character/isLetterOrDigit ch)))

(defn letter-digit-period? [^Character ch]
  (or (= \_ ch)
      (= \. ch)
      (= \+ ch)
      (= \- ch)
      (Character/isLetterOrDigit ch)))

(defn digit? [^Character ch]
  (Character/isDigit ch))

(defn newline? [ch]
  (= \newline ch))

(defn whitespace? [^Character ch]
  (Character/isWhitespace ch))

(defn matches-regex [re s]
  (not (nil? (re-matches re s))))

(def unsigned-long-re-without-wordbreak #"[0-9]+([lL][uU]|[uU][lL])")
(def signed-long-re-without-wordbreak #"[0-9]+[lL]")
(def unsigned-int-re-without-wordbreak #"[0-9]+[uU]")
(def signed-int-re-without-wordbreak #"[0-9]+")
(def floating-point-constant-without-wordbreak #"([0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\.")

(def unsigned-long-re #"([0-9]+([lL][uU]|[uU][lL]))[^\w.]")
(def signed-long-re #"([0-9]+[lL])[^\w.]")
(def unsigned-int-re #"([0-9]+[uU])[^\w.]")
(def signed-int-re #"([0-9]+)[^\w.]")
(def floating-point-constant #"(([0-9]*\.[0-9]+|[0-9]+\.?)[Ee][+-]?[0-9]+|[0-9]*\.[0-9]+|[0-9]+\.)[^\w.]")

(defn- re-find-indexed [re s]
  (let [matcher (re-matcher re s)]
    (when (.find matcher)
      [(.group matcher 1)
       (.start matcher 1)
       (.end matcher 1)])))

(defn match-regex
  "Returns matched string and remaining string tuple, otherwise returns nil.

  The first match by re-finds must be the starting subsequence, otherwise false."
  [re s]
  (when-let [[matched start-index _] (re-find-indexed re s)]
    (when (and (= 0 start-index) (str/starts-with? s matched))
      [matched (str/replace-first s matched "")])))

(defn read-number
  "Returns tuple of matched number and remaining string, otherwise nil."
  [s line col]
  (if-let [x (or
              (match-regex floating-point-constant s)
              (match-regex signed-int-re s)
              (match-regex signed-long-re s)
              (match-regex unsigned-int-re s)
              (match-regex unsigned-long-re s))]
    x
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

(defn type-double? [t]
  (= {:type :double} t))

(defn type-signed? [t]
  (condp = t
    {:type :int} true
    {:type :long} true
    {:type :uint} false
    {:type :ulong} false
    (exc/analyzer-error "Invalid type passed to type-signed?." {:type t})))
