(defproject penumbra "0.5.0-SNAPSHOT"
  :description "An idiomatic wrapper for OpenGL"
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
                 [slick-util "1.0.0"]]
  :native-dependencies [[lwjgl "2.2.2"]]
  :dev-dependencies [[native-deps "1.0.0"]
                     [leiningen/lein-swank "1.1.0"]])