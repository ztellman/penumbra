(ns example.wiki.triangle3
  (:use [penumbra.opengl])
  (:require [penumbra.app :as app]))

(defn init [state]
  (app/vsync! true)
  state)

(defn reshape [[x y width height] state]
  (frustum-view 60.0 (/ (double width) height) 1.0 100.0)
  (load-identity)
  state)

(defn mouse-drag [[dx dy] [x y] button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(defn display [[delta time] state]
  (translate 0 -0.93 -3)
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (draw-triangles
    (color 1 0 0) (vertex 1 0)
    (color 0 1 0) (vertex -1 0)
    (color 0 0 1) (vertex 0 1.86)))

(app/start 
  {:display display, :reshape reshape, :mouse-drag mouse-drag, :init init} 
  {:rot-x 0, :rot-y 0})