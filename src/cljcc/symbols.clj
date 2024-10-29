(ns cljcc.symbols)

(def symbols
  "Holds global symbol table.

  Maps identifiers to their types."
  (atom {}))

(defn reset-symbols [new-symbols]
  (reset! symbols new-symbols))
