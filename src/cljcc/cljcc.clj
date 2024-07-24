(ns cljcc.cljcc
  (:require
   [cljcc.driver :as d])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn -main
  "Main entrypoint for cljcc compiler."
  [& args]
  (let [input-file-path (first args)]
    (try
     (d/run input-file-path)
     (println "success")
     (catch Exception e
       (println "Error: " (.getMessage e))
       (System/exit 1))
     (finally
       (System/exit 0)))))
