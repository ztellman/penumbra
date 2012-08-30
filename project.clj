(defproject penumbra "0.6.0"
  :description "An idiomatic wrapper for OpenGL"
  :dependencies [[slick-util "1.0.0"]
                 [cantor "0.3.0"]
                 [org.clojure/clojure "1.2.0"] 
                 [org.clojure/clojure-contrib "1.2.0"]
				 [org.clojars.charles-stain/lwjgl "3.0"]
				 [org.lwjgl/lwjgl-util "2.7.1"]
				 [org.clojars.charles-stain/jme3-lwjgl-natives "3.0"]]
  :java-source-paths ["java"]
  :dev-dependencies [[swank-clojure "1.3.0"]])