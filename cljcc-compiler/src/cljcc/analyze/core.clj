(ns cljcc.analyze.core
  (:require [cljcc.analyze.resolve :as r]
            [cljcc.analyze.label-loops :as l]
            [cljcc.analyze.typecheck :as t]))

(defn validate [program]
  (-> program
      r/resolve-program
      l/label-loops
      t/typecheck))
