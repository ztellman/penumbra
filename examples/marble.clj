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
     'penumbra.opengl.view
     'penumbra.opengl.texture
     'penumbra.opengl.shader
     'penumbra.interface.window)

;;;;;;;;;;;;;;;;;

(defn textured-quad []
  (push-matrix
    (translate -0.5 -0.5 0.5)
    (normal 0 0 -1)
    (draw-quads
      (texture 1 1) (vertex 1 1 0)
      (texture 0 1) (vertex 0 1 0)
      (texture 0 0) (vertex 0 0 0)
      (texture 1 0) (vertex 1 0 0))))

(def declarations
  '(varying vec4 pos))

(def vertex-shader
  '((= pos :vertex)
    (= :position (ftransform))))

(def fragment-shader
  '(= :frag-color (vec4 (abs (.x pos)) (abs (.y pos)) 0 1)))

;;;;;;;;;;;;;;;;;

(defn init [state]
  (enable :depth-test)
  (let [program (create-program declarations vertex-shader fragment-shader)]
    (bind-program program))
  state)

(defn reshape [[x y w h] state]
  (frustum-view 90 (/ w (float h)) 0.1 10)
  (load-identity)
  (translate 0 0 -2)
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
