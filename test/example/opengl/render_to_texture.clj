;;   Copyright (c) Zachary Tellman. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns example.opengl.render-to-texture
  (:use [penumbra opengl])
  (:require [penumbra.app :as app]
            [penumbra.data :as data]))

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

(defn create-checkers []
  (let [tex (create-byte-texture 128 128)]
    (data/overwrite!
     tex
     (apply concat
            (for [x (range 128) y (range 128)]
              (if (xor (even? (bit-shift-right x 4)) (even? (bit-shift-right y 4)))
                [1 0 0 1]
                [0 0 0 1]))))
    tex))

;;;

(defn init [state]
  (app/title! "Render to Texture")
  (tex-env :texture-env :texture-env-mode :modulate)
  (enable :texture-2d)
  (enable :depth-test)
  (enable :light0)
  (enable :lighting)
  (line-width 3)
  (assoc state
    :checkers (create-checkers)
    :view (create-byte-texture 256 256)))

(defn reshape [_ state]
  (load-identity)
  (scale 1 1 -1)
  (translate 0 0 2))

(defn mouse-drag [[dx dy] [x y] button state]
  (let [[w h] (app/size)]
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
        [w h] (app/size)]

    (light 0
       :position [-1 -1 1 0])
    (material :front-and-back
       :ambient-and-diffuse [0.8 0.1 0.1 1])

    ;;render the checkered cube to a texture
    (render-to-texture view
     (clear 0.5 0.5 0.5)
     (with-projection (frustum-view 50. 1. 0.1 10.)
       (push-matrix
        (rotate lx 1 0 0) (rotate ly 0 1 0)
        (with-texture checkers
          (textured-cube)))))

    (clear 0 0 0)

    (with-projection (frustum-view 90. (double (/ w 2.0 h)) 0.1 10.)
      ;;render the checkered cube to the window
      (with-texture checkers
        (with-viewport [0 0 (/ w 2.0) h]
          (push-matrix
           (rotate lx 1 0 0) (rotate ly 0 1 0)
           (textured-cube))))
      ;;render a cube with the checkered cube texture
      (with-texture view
        (with-viewport [(/ w 2.0) 0 (/ w 2.0) h]
          (push-matrix
           (rotate rx 1 0 0) (rotate ry 0 1 0)
           (textured-cube)))))
    
    ;;draw a dividing line
    (with-disabled [:texture-2d :lighting]
      (with-projection (ortho-view 0 1 0 1 0 1)
        (push-matrix
         (load-identity)
         (draw-lines (vertex 0.5 0) (vertex 0.5 1)))))))

(defn start []
  (app/start
   {:display display, :mouse-drag mouse-drag, :reshape reshape, :init init}
   {:left [0 0], :right [0 0], :checkers nil, :view nil}))



