(ns penumbra.examples.gpgpu
  (:use [penumbra.interface.slate])
  (:use [penumbra.opengl.core])
  (:use [penumbra.compute.core]))

(def slate (create-slate 100 100))

(with-slate slate
  (println "hello")
  (println "goodbye"))
