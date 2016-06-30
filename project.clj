(defproject penumbra "0.6.1"
  :description "An idiomatic wrapper for OpenGL"
  :dependencies [[slick-util "1.0.0"]
                 [cantor "0.3.0"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/math.combinatorics "0.1.3"]
;                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojars.charles-stain/lwjgl "3.0"]
                 [org.lwjgl/lwjgl-util "2.7.1"]
                 [org.clojars.charles-stain/jme3-lwjgl-natives "3.0"]]
  :java-source-paths ["java"]
  :plugins [[lein-localrepo "0.5.3"]]
  :profiles {:dev {:source-paths ["src"
                                  "test"]}}
  ;:dev-dependencies [[swank-clojure "1.3.0"]]
  )
