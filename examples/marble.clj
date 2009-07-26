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

(def declarations
  '((varying float noise)
    (varying vec4 pos)
    (varying vec3 normal)
    (varying vec4 intensity)))

(def vertex-shader
  '((import (penumbra.opengl.effect lighting directional-light))
    (let [pos         :vertex ;(* :model-view-matrix :vertex)
          noise       (* 1.5 (noise1 pos)) ;(* 8.0 (noise1 pos))
          normal      (normalize (* :normal-matrix :normal))
          intensity   (lighting 0 normal)]
      (set! :position (* :model-view-projection-matrix :vertex)))))

(def fragment-shader
  '(let [(float marble)       (-> pos .x (* 2.0) (+ noise) sin abs)
         (vec4 marble-color)  (vec4 0.8 0.7 0.7 1.0)
         (vec4 vein-color)    (vec4 0.2 0.15 0.1 1.0)
         (vec4 color)         (mix vein-color marble-color (pow marble 0.5))]
      (set! :frag-color color)))

;;;;;;;;;;;;;;;;;;
;quad

(defn reshape-quad [[x y w h] state]
  (frustum-view 60 (/ w (float h)) 0.1 10)
  (load-identity)
  (translate 0 0 -3)
  state)

(defn subdivided-quad []
  (push-matrix
    (translate -0.5 -0.5 0)
    (dotimes [y 50]
      (let [y1 (/ y 50.)
            y2 (/ (inc y) 50.)]
        (draw-quad-strip
          (vertex 0 y1 0)
          (vertex 0 y2 0)
          (dotimes [x 50]
            (vertex (/ (inc x) 50.) y1 0)
            (vertex (/ (inc x) 50.) y2 0)))))))

(defn init-quad [state]
  (let [program (create-program declarations vertex-shader fragment-shader)]
    (bind-program program))
  (assoc state
    :quad (get-display-list (subdivided-quad))))

(defn mouse-drag-quad [[[dx dy] _] state]
  (assoc state
    :z-offset (+ (:z-offset state) (/ dy 100))))

(defn display-quad [[delta time] state]
  (translate 0 0 (:z-offset state))
  (call-display-list (:quad state)))

'(start
  {:reshape reshape-quad, :display display-quad, :init init-quad, :mouse-drag mouse-drag-quad}
  {:z-offset 0, :quad nil})

;;;;;;;;;;;;;;;;;
;teapot

(defn reshape-teapot [[x y w h] state]
  (frustum-view 60 (/ w (float h)) 0.1 10)
  (load-identity)
  (translate 0 0 -3)
  (light 0
    :position [1 1 1 0])
  (material :front-and-back
    :ambient-and-diffuse [1 1 1 1]
    :specular            [0.5 0.4 0.4 1]
    :shininess           64)
  state)

(defn init-teapot [state]
  (enable :depth-test)
  (enable :lighting)
  (enable :light0)
  (let [program (create-program declarations vertex-shader fragment-shader)]
    (bind-program program))
  state)

(defn mouse-drag-teapot [[[dx dy] _] state]
  (assoc state
    :rot-x (- (:rot-x state) dy)
    :rot-y (- (:rot-y state) dx)))

(defn display-teapot [[delta time] state]
  (rotate (:rot-x state) 1 0 0)
  (rotate (:rot-y state) 0 1 0)
  (teapot))

'(start
  {:reshape reshape-teapot, :display display-teapot, :init init-teapot, :mouse-drag mouse-drag-teapot}
  {:rot-x 0 :rot-y 0})
