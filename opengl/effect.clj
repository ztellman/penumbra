;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.effect)

(use 'penumbra.opengl.core
     'penumbra.opengl.geometry)

(import '(java.awt Font)
        '(com.sun.opengl.util.j2d TextRenderer))

;;;;;;;;;;;;;;;;;;;;;;;

(gl-import glColor3d color)
(gl-import glOrtho gl-ortho)
(glu-import gluPerspective glu-perspective)
(gl-import glCullFace gl-cull-face)
(gl-import glPolygonMode gl-polygon-mode)
(gl-import glClearColor clear-color)
(gl-import glLightfv set-light)
(gl-import glMaterialfv set-material)
(gl-import glFogfv gl-fog)
(gl-import glShadeModel shade-model)
(gl-import glLineWidth line-width)

;;;;;;;;;;;;;;;;;;;;;;;

(defmacro set-light-position [num [x y z w]]
  (let [light# (keyword (str "light" num))]
    `(do
      (enable :lighting)
      (enable ~light#)
      (set-light ~light# :position (float-array 4 [~x ~y ~z ~w]) 0))))

(defmacro setup-fog [mode density near far [r g b a]]
  `(do
    (enable :fog)
    (gl-fog :fog-mode (float-array 1 [~(translate-keyword mode)]) 0)
    (gl-fog :fog-density (float-array 1 [~density]) 0)
    (gl-fog :fog-start (float-array 1 [~near]) 0)
    (gl-fog :fog-end (float-array 1 [~far]) 0)
    (gl-fog :fog-color (float-array 4 [~r ~g ~b ~a]) 0)))

(defn material [r g b a]
  (set-material :front-and-back :ambient-and-diffuse (float-array 4 [r g b a]) 0))

(defn draw-solid [] (gl-polygon-mode :front-and-back :fill))
(defn draw-wireframe [] (gl-polygon-mode :front-and-back :line))
(defn draw-point-cloud [] (gl-polygon-mode :front-and-back :point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-projection [projection & body]
  `(do
    (gl-matrix-mode :projection) (gl-push-matrix) ~projection (gl-matrix-mode :modelview)
    ~@body
    (gl-matrix-mode :projection) (gl-pop-matrix) (gl-matrix-mode :modelview)))

(defn ortho-view
  "Create orthographic view, where distant objects don't get smaller."
  [left top right bottom near far]
  (gl-matrix-mode :projection)
  (load-identity)
  (gl-ortho left right bottom top near far)
  (gl-matrix-mode :modelview))

(defn frustum-view [fovx aspect near far]
  "Create a standard perspective view."
  (gl-matrix-mode :projection)
  (load-identity)
  (glu-perspective fovx aspect near far)
  (gl-matrix-mode :modelview))

;;;;;;;;;;;;;;;;;;;;;;;;;;

(def #^TextRenderer *text* (TextRenderer. (Font. "Tahoma" java.awt.Font/PLAIN 20) true true))

(defn write
  "writes string at normalized coordinates (x,y)"
  [string x y]
  (let [[_ _ w h] @view-bounds
        text-height (.. *text* (getBounds string) getHeight)]
    (.beginRendering *text* w h)
    (.draw *text* string (int (* x w)) (int (* y (- h text-height))))
    (.endRendering *text*)))


