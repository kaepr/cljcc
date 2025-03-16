(ns cljcc.core.log
  (:require [clojure.string :as str]))

(def ^:private log-colors
  {:debug "\u001b[36m" ; Cyan
   :info  "\u001b[32m" ; Green
   :warn  "\u001b[33m" ; Yellow
   :error "\u001b[31m" ; Red
   :reset "\u001b[0m"}) ; Reset color

(def reset-color (get log-colors :reset))

(defn- log-message [level message]
  (let [color (get log-colors level)
        formatted-message (str color "[" (str/upper-case (name level)) "] " message reset-color)]
    (println formatted-message)))

(defn debug [msg]
  (log-message :debug msg))

(defn info [msg]
  (log-message :info msg))

(defn warn [msg]
  (log-message :warn msg))

(defn error [msg]
  (log-message :error msg))
