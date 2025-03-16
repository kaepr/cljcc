(ns cli.cli
  (:require
   [clojure.tools.cli :refer [parse-opts]]
   [clojure.string :as string]
   [cli.core.shell :refer [exit]]
   [cli.driver :as driver])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn usage [options-summary]
  (->>
   ["Usage: ./cljcc-cli path/to/file.c [options]"
    ""
    "Options:"
    options-summary]
   (string/join \newline)))

(def cli-options
  [[nil "--lex" "Runs lexer. Does not emit any files."]
   [nil "--parse" "Runs parser. Does not emit any files."]
   [nil "--validate" "Runs semantic analyzer. Does not emit any files."]
   [nil "--tacky" "Runs tacky generation. Does not emit any files."]
   [nil "--codegen" "Runs compiler. Does not emit any files."]
   ["-c" nil "Generate object file."
    :id :generate-object-file]
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
  (let [{:keys [file-path exit-message ok? options]} (validate-args args)
        libs (filterv (fn [v] (and
                               (string? v)
                               (re-matches #"-l.+" v)))
                      args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (try
        (driver/run file-path (assoc options :libs libs))
        (exit 0 "Successfully executed.")
        (catch Exception e
          (exit 1 (ex-message e) e))))))
