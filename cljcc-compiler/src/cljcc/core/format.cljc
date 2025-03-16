(ns cljcc.core.format
  #?(:clj (:require [clojure.core :refer [format]])
     :cljs (:require [goog.string :as gstring]
                     [goog.string.format])))

(defn safe-format
  "Cross platform format."
  [fmt & args]
  #?(:clj (apply format fmt args)
     :cljs (apply gstring/format fmt args)))
