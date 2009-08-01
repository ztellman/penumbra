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
     'penumbra.opengl.effect
     'penumbra.opengl.texture
     'penumbra.interface.window)

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
  (let [view (create-texture 256 256)
        checkers (create-texture 128 128)]
    (time (draw-to-subsampled-texture
      checkers
      (fn [[x y] _]
        (if (xor (even? (bit-shift-right x 4)) (even? (bit-shift-right y 4)))
          [1 0 0 1]
          [0 0 0 1]))))
    [checkers view]))

;;;;;;;;;;;;;;;;;

(defn init [state]
  (tex-env :texture-env :texture-env-mode :modulate)
  (enable :texture-2d)
  (enable :normalize)
  (enable :multisample)
  (enable :depth-test)
  (line-width 3)
  (let [[checkers view] (init-textures)]
    (assoc state
      :checkers checkers
      :view view)))

(defn reshape [_ state]
  (load-identity)
  (scale 1 1 -1)
  (translate 0 0 2)
  state)

(defn mouse-drag [mouse state]
  (let [[delta pos] mouse
        [dx dy] delta
        [x y] pos
        [w h] (get-canvas-size)]
    (if (< x (int (/ w 2)))
      (let [[lx ly] (:left state)]
        (assoc state :left [(- lx dy) (- ly dx)]))
      (let [[rx ry] (:right state)]
        (assoc state :right [(- rx dy) (- ry dx)])))))


(defn display [[delta time] state]
  (let [[lx ly] (:left state)
        [rx ry] (:right state)
        checkers (:checkers state)
        view (:view state)
        [w h] (get-canvas-size)]

    (enable :texture-2d)
    (enable :light0)
    (enable :lighting)
    (light 0
      :position [-1 -1 1 0])
    (material :front-and-back
      :ambient-and-diffuse [0.8 0.1 0.1 1])

    (bind-texture checkers)

    ;render the checkered cube to a texture
    (clear-color 0.5 0.5 0.5 1)
    (render-to-texture view
      (with-projection (frustum-view 50 1 0.1 10)
        (push-matrix
          (rotate lx 1 0 0) (rotate ly 0 1 0)
          (textured-cube))))

    (clear-color 0 0 0 1)
    (clear)

    (with-projection (frustum-view 90 (float (/ w 2.0 h)) 0.1 10)
      ;render the checkered cube to the window
      (bind-texture checkers)
      (with-viewport [0 0 (/ w 2.0) h]
        (push-matrix
          (rotate lx 1 0 0) (rotate ly 0 1 0)
          (textured-cube)))
      ;render a cube with the checkered cube texture
      (bind-texture view)
      (with-viewport [(/ w 2.0) 0 (/ w 2.0) h]
        (push-matrix
          (rotate rx 1 0 0) (rotate ry 0 1 0)
          (textured-cube))))

    ;draw a dividing line
    (disable :lighting)
    (disable :texture-2d)
    (with-projection (ortho-view 0 0 1 1 0 10)
      (draw-lines (vertex 0.5 0 5) (vertex 0.5 1 5)))))

(start
  {:display display, :mouse-drag mouse-drag, :reshape reshape, :init init}
  {:left [0 0], :right [0 0], :checkers nil, :view nil})



