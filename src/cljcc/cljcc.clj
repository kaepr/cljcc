(ns cljcc.cljcc
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   [clojure.string :as string]
   [cljcc.log :as log]
   [cljcc.util :refer [exit]]
   [cljcc.driver :as d])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn usage [options-summary]
  (->>
   ["Usage: ./cljcc path/to/file.c [options]"
    ""
    "Options:"
    options-summary]
   (string/join \newline)))

(def cli-options
  [[nil "--parse" "Runs parser. Does not emit any files."]
   [nil "--codegen" "Runs compiler. Does not emit any files."]
   ["-h" "--help"]])

(defn validate-args [args]
  (let [{:keys [options arguments summary]} (parse-opts args cli-options)]
    (cond
      (:help options) {:exit-message (usage summary) :ok? true}
      (= 1 (count arguments)) {:file-path (first arguments)
                               :options options}
      :else {:exit-message (usage summary)})))

(defn -main
  "Main entrypoint for cljcc compiler."
  [& args]
  (let [{:keys [file-path exit-message ok? options]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (try
       (d/run file-path options)
       (catch Exception e (exit 1 (.getMessage e)))
       (finally (exit 0 "Succesfully ran compiler."))))))
