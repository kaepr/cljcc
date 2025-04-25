(ns cljcc.cljcc
  (:require
   [cljcc.lexer :as lexer]
   [cljcc.parser :as parser]
   [cljcc.tacky :as tacky]
   [cljcc.config :as config]
   [cljcc.analyze.core :as analyzer]
   [cljcc.compiler :as codegen]
   [cljcc.emit :as emit])
  (:gen-class))

(set! *warn-on-reflection* true)

(def valid-os-targets #{:mac :linux})
(def valid-stages #{:lex :parse :validate :tacky :codegen :emit})

(defn run
  "Compiles source input using specified compiler options.

  Parameters:
    source - Source C file.
    options - Map of compiler configuration options.

  Options map:
    :target - Map of target platform settings
      :os - Target OS #{:linux :mac}
    :stage - Which stage to generate #{:lex :parse :validate :tacky :codegen :emit}

  Returns generated AST for specified stage."
  [source & {:keys [options] :or {options {}}}]
  (let [default-options {:target {:os :linux}
                         :stage :emit}
        merged-options (merge default-options options)
        stage (:stage merged-options)
        target-os (:os (:target merged-options))
        _ (assert (stage valid-stages) "Invalid stage for compilation.")
        _ (assert (target-os valid-os-targets) "Invalid operating system.")
        _ (config/set-os target-os)
        stages [lexer/lex parser/parse
                analyzer/validate tacky/tacky-generate
                codegen/assembly emit/emit]
        stage-idx (condp = stage
                    :lex 1
                    :parse 2
                    :validate 3
                    :tacky 4
                    :codegen 5
                    :emit 6)
        stages-to-run (vec (take stage-idx stages))]
    (reduce (fn [acc f] (f acc)) source stages-to-run)))

(defn- validate-os [os]
  (let [valid-os #{"mac" "linux"}]
    (assert (valid-os os) (str "OS: " os "is not valid." "OS type is not valid. Valid os: " (vec valid-os))))
  os)

(defn- validate-stage [stage]
  (let [valid-stages #{"lex" "parse" "validate" "tacky" "codegen" "emit"}]
    (assert (valid-stages stage) (str "Stage is not valid. Valid stages: " (vec valid-stages)))
    stage))

(defn -main [& args]
  (let [[source os stage] args
        source (or source "")
        os (or os "")
        stage (or stage "")]
    (validate-os os)
    (validate-stage stage)
    (println (run source {:options {:os (keyword os)
                                    :stage (keyword stage)}}))))

(comment

  (-main "int main(void) {return 42;}" "linux" "lex"))
