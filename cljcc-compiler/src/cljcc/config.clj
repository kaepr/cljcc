(ns cljcc.config)

(def config (atom {}))

(defn set-os [os]
  (swap! config assoc :os os))

(defn get-os []
  (or (get @config :os) :linux))
