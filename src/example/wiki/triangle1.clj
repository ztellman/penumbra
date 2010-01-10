(ns example.wiki.triangle1
  (:use [penumbra.opengl])
  (:require [penumbra.app :as app]))

(defn reshape [[x y width height] state]
  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  state)

(defn display [[delta time] state]
  (translate 0 -0.93 -3)
  (draw-triangles
    (color 1 0 0) (vertex 1 0)
    (color 0 1 0) (vertex -1 0)
    (color 0 0 1) (vertex 0 1.86)))

(app/start 
  {:display display, :reshape reshape} 
  {})