(ns cljcc.cljcc
  (:require
   [cljcc.driver :as d])
  (:gen-class))

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn -main
  "I don't do a whole lot ... yet."
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
