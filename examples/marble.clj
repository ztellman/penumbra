;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.examples.marble)

(use 'penumbra.opengl.core
     'penumbra.opengl.geometry
     'penumbra.opengl.effect
     'penumbra.opengl.shader
     'penumbra.interface.window)

;;;;;;;;;;;;;;;;;

(defn quad []
  (push-matrix
    (normal 0 0 -1)
    (translate -0.5 -0.5 0)
    (draw-quads
      (vertex 1 1 0)
      (vertex 0 1 0)
      (vertex 0 0 0)
      (vertex 1 0 0))))

(def declarations
  '((varying float noise)
    (varying vec4 pos)
    (varying vec3 normal)
    (varying vec4 intensity)))

(def vertex-shader
  '((import (penumbra.opengl.effect lighting directional-light))
    (let [noise       (noise1 :vertex)
          pos         :vertex
          normal      (normalize (* :normal-matrix :normal))
          intensity   (lighting 0 normal)]
      (set! :position (* :model-view-projection-matrix :vertex)))))

(def fragment-shader
  '(let [(float marble)      (abs (+ (sin (+ (* (.x pos) 2.0) noise)) (cos (+ (.x pos) noise))))
          (vec4 marble-color) (vec4 0.8 0.7 0.7 1.0)
          (vec4 vein-color)   (vec4 0.2 0.15 0.1 1.0)
          (vec4 color)        (mix vein-color marble-color (pow (clamp marble 0.0 1.0) 0.75))]
      (set! :frag-color (* intensity color))))

;;;;;;;;;;;;;;;;;

(defn init [state]
  (enable :depth-test)
  (enable :lighting)
  (enable :light0)
  (let [program (create-program declarations vertex-shader fragment-shader)]
    (bind-program program))
  state)

(defn reshape [[x y w h] state]
  (frustum-view 60 (/ w (float h)) 0.1 10)
  (load-identity)
  (translate 0 0 -3)
  (light 0
    :position [1 1 1 0])
  (material :front-and-back
    :ambient-and-diffuse [1 1 1 1]
    :specular            [0.3 0.3 0.3 1]
    :shininess           64)
  state)

(defn mouse-drag [[[dx dy] _] state]
  (assoc state
    :rot-x (- (:rot-x state) dy)
    :rot-y (- (:rot-y state) dx)))

(defn display [[delta time] state]
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (teapot))

(start
  {:reshape reshape, :display display, :init init, :mouse-drag mouse-drag}
  {:rot-x 0 :rot-y 0})
