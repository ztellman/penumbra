;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.opengl.sierpinski
  (:use [penumbra opengl])
  (:require [penumbra.app :as app]))

(defn draw-pyramid []
  (material :front-and-back
    :ambient-and-diffuse [1 0.25 0.25 1])
  (draw-triangle-fan
    (vertex 0 1 0)
    (dotimes [_ 5]
      (rotate 90 0 1 0)
      (normal 1 0.5 1)
      (vertex 0.5 0 0.5)))
  (draw-quads
    (normal 0 -1 0)
    (dotimes [_ 4]
      (rotate -90 0 1 0)
      (vertex 0.5 0 0.5))))

(defn subdivide [display-list]
  (push-matrix
    (scale 0.5 0.5 0.5)
    (push-matrix
      (translate 0 1 0)
      (display-list))
    (dotimes [_ 4]
      (rotate 90 0 1 0)
      (push-matrix
        (translate 0.5 0 0.5)
        (display-list)))))

(defn sierpinski []
  (iterate
   #(create-display-list (subdivide %))
   (create-display-list (draw-pyramid))))

;;;;;;;;;;;;;;;;;

(defn init [state]
  (app/title! "Sierpinski Pyramid")
  (enable :normalize)
  (enable :depth-test)
  (enable :cull-face)
  (enable :lighting)
  (enable :light0)
  (enable :fog)
  (shade-model :flat)
  (assoc state :pyramid (nth (sierpinski) 5)))

(defn reshape [[x y width height] state]
  (frustum-view 50 (/ (double width) height) 0.1 100)
  (load-identity)
  (translate 0 -0.35 -1.75)
  (light 0
    :position [1 1 1 0])
  (fog
    :fog-mode :exp
    :fog-density 0.75
    :fog-start 0
    :fog-end 10
    :fog-color [0 0 0 0])
  state)

(defn mouse-drag [[dx dy] _ button state]
  (assoc state
    :rot-x (+ (:rot-x state) dy)
    :rot-y (+ (:rot-y state) dx)))

(defn display [[delta time] state]
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  ((:pyramid state)))

(defn start []
  (app/start
   {:display display, :mouse-drag mouse-drag, :reshape reshape, :init init}
   {:rot-x 0, :rot-y 0, :pyramid nil}))



