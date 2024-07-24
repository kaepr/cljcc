(ns cljcc.util
  (:require [clojure.java.shell :refer [sh]]))

(defn get-os []
  (let [os-name (.toLowerCase (System/getProperty "os.name"))]
    (cond
      (.contains os-name "mac") :mac
      (.contains os-name "linux") :linux
      :else :unsupported)))

(defn mac-aarch64? []
  (and (= :mac (get-os)) (= (System/getProperty "os.arch" "aarch64"))))

(defn handle-sh
  "Preprends arch -x86_64 if running under Mac M chips."
  [command & args]
  (if (mac-aarch64?)
    (apply sh "arch" "-x86_64" command args)
    (apply sh command args)))
