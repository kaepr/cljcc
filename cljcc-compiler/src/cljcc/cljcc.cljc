(ns cljcc.cljcc
  (:require
   [cljcc.lexer :as lexer]
   [cljcc.parser :as parser]
   [cljcc.tacky :as tacky]
   [cljcc.analyze.core :as analyzer]
   [cljcc.compiler :as codegen]
   [cljcc.emit :as emit])
  (:gen-class))

#?(:clj  (set! *warn-on-reflection* true))

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

