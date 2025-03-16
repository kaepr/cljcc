(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.tools.build.api :as b]))

(def lib 'net.clojars.cljcc-cli/cljcc-cli)
(def main 'cli.cli)
(def class-dir "../target/classes")

(defn- uber-opts [opts]
  (assoc opts
         :lib lib :main main
         :uber-file "../target/cli/cljcc-cli.jar"
         :basis (b/create-basis {})
         :class-dir class-dir
         :src-dirs ["src"]
         :ns-compile [main]))

(defn ci "Run the CI pipeline of tests (and build the uberjar)." [opts]
  (b/delete {:path "../target"})
  (let [opts (uber-opts opts)]
    (println "\nCopying source...")
    (b/copy-dir {:src-dirs ["resources" "src"] :target-dir class-dir})
    (println (str "\nCompiling " main "..."))
    (b/compile-clj opts)
    (println "\nBuilding JAR...")
    (b/uber opts)
    (println "\nJAR built."))
  opts)
