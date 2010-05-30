(defproject penumbra "0.6.0-SNAPSHOT"
  :description "An idiomatic wrapper for OpenGL"
  :dependencies [[slick-util "1.0.0"]
<<<<<<< HEAD:project.clj
                 [cantor "0.2.0-SNAPSHOT"]
=======
                 [cantor "0.1.0"]
>>>>>>> a5045ad609301cde724c2ca32b1596e1af5bff48:project.clj
                 [org.clojure/clojure "1.2.0-master-SNAPSHOT"] 
                 [org.clojure/clojure-contrib "1.2.0-SNAPSHOT"]]
  :native-dependencies [[penumbra/lwjgl "2.4.2"]]
  :dev-dependencies [[native-deps "1.0.0"]
					 [autodoc "0.7.1"]
                     [lein-clojars "0.5.0-SNAPSHOT"]
                     [leiningen/lein-swank "1.1.0"]])