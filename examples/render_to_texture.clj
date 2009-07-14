;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.examples.render-to-texture)

(use 'penumbra.opengl.core
     'penumbra.opengl.geometry
     'penumbra.opengl.view
     'penumbra.opengl.texture
     'penumbra.interface.window)

(def checkers (atom nil))
(def view (atom nil))

(def left-rot-x (ref 0))
(def left-rot-y (ref 0))

(def right-rot-x (ref 0))
(def right-rot-y (ref 0))

(defn textured-quad []
  (push-matrix
    (translate -0.5 -0.5 0.5)
    (normal 0 0 -1)
    (draw-quads
      (texture 1 1) (vertex 1 1 0)
      (texture 0 1) (vertex 0 1 0)
      (texture 0 0) (vertex 0 0 0)
      (texture 1 0) (vertex 1 0 0))))

(defn textured-cube []
  (dotimes [_ 4]
    (rotate 90 0 1 0)
    (textured-quad))
  (rotate 90 1 0 0)
  (textured-quad)
  (rotate 180 1 0 0)
  (textured-quad))

(defn xor [a b] (or (and a (not b)) (and (not a) b)))

(defn init-textures []
  (reset! view (create-texture 256 256))
  (reset! checkers (create-texture 128 128))
  (draw-to-subsampled-texture
    @checkers
    (fn [[x y] _]
      (if (xor (even? (int (/ x 16))) (even? (int (/ y 16))))
        [1 0 0 1]
        [0 0 0 1]))))

(defn init []
  (tex-env :texture-env :texture-env-mode :modulate)
  (enable :texture-2d)
  (enable :normalize)
  (enable :multisample)
  (enable :depth-test)
  (init-textures))

(defn reshape [x y width height]
  (load-identity)
  (scale 1 1 -1)
  (translate 0 0 2))

(defn mouse-drag [[dx dy] [x y]]
  (let [[_ _ w h] @view-bounds]
    (if (< x (int (/ w 2)))
      (dosync
        (ref-set left-rot-x (- @left-rot-x dy))
        (ref-set left-rot-y (- @left-rot-y dx)))
      (dosync
        (ref-set right-rot-x (- @right-rot-x dy))
        (ref-set right-rot-y (- @right-rot-y dx))))))

(defn display [delta time]
  (bind-texture @checkers)
  (enable :texture-2d)

  (set-light-position 0 [-1 -1 1 0])
  (material 0.8 0.1 0.1 1)

  ;render the checkered cube to a texture
  (clear-color 0.5 0.5 0.5 1)
  (render-to-texture @view
    (with-projection (frustum-view 50 1 0.1 10)
      (push-matrix
        (rotate @left-rot-x 1 0 0) (rotate @left-rot-y 0 1 0)
        (textured-cube))))

  (clear-color 0 0 0 1)
  (clear)

  (let [[_ _ w h] @view-bounds]
    (with-projection (frustum-view 90 (float (/ w 2.0 h)) 0.1 10)
      ;render the checkered cube to the window
      (bind-texture @checkers)
      (with-viewport [0 0 (/ w 2.0) h]
        (push-matrix
          (rotate @left-rot-x 1 0 0) (rotate @left-rot-y 0 1 0)
          (textured-cube)))
      ;render a cube with the checkered cube texture
      (bind-texture @view)
      (with-viewport [(/ w 2.0) 0 (/ w 2.0) h]
        (push-matrix
          (rotate @right-rot-x 1 0 0) (rotate @right-rot-y 0 1 0)
          (textured-cube))))

    ;draw a dividing line
    (disable :lighting)
    (disable :texture-2d)
    (color 1 1 1)
    (line-width 3)
    (with-projection (ortho-view 0 0 1 1 0 10)
      (draw-lines (vertex 0.5 0 5) (vertex 0.5 1 5)))))

(start {:display display, :mouse-drag mouse-drag, :reshape reshape, :init init})



