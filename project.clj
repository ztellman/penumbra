(defproject penumbra "0.5.0-SNAPSHOT"
  :description "Penumbra is an idiomatic wrapper for OpenGL in Clojure, by way of LWJGL."
  :dependencies [[org.clojure/clojure "1.1.0-new-SNAPSHOT"]
                 [org.clojure/clojure-contrib "1.0-SNAPSHOT"]]
  ;; TODO: native dependencies flag is needed here to support
  ;; downloading and extracting lwjgl with lein deps.
  ;; Should add as soon as native dependencies flag
  ;; is implemented in leiningen.
  :dev-dependencies [[leiningen/lein-swank "1.1.0"]])