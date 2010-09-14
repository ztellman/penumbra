(defproject penumbra "0.6.0-SNAPSHOT"
  :description "An idiomatic wrapper for OpenGL"
  :dependencies [[slick-util "1.0.0"]
                 [cantor "0.2.1"]
                 [org.clojure/clojure "1.2.0"] 
                 [org.clojure/clojure-contrib "1.2.0"]]
  :native-dependencies [[penumbra/lwjgl "2.4.2"]]
  :dev-dependencies [[native-deps "1.0.4"]
                     [autodoc "0.7.1"]
                     [lein-clojars "0.5.0-SNAPSHOT"]
                     [leiningen/lein-swank "1.1.0"]])
