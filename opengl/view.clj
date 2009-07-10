(ns penumbra.opengl.view)

(use 'penumbra.opengl.core 'penumbra.opengl.geometry)

(import '(java.awt Font)
        '(com.sun.opengl.util.j2d TextRenderer))

(def view-dim (ref [0 0]))

;;;;;;;;;;;;;;;;;;;;;;;

(gl-import glColor3d color)
(gl-import glOrtho gl-ortho)
(glu-import gluPerspective glu-perspective)
(gl-import glCullFace gl-cull-face)
(gl-import glPolygonMode gl-polygon-mode)
(gl-import glClear gl-clear)
(gl-import glClearColor clear-color)
(gl-import glLightfv set-light)
(gl-import glMaterialfv set-material)
(gl-import glFogfv gl-fog)
(gl-import glShadeModel shade-model)
(gl-import glViewport gl-viewport)

;;;;;;;;;;;;;;;;;;;;;;;

(defn clear []
  (gl-clear :depth-buffer-bit)
  (gl-clear :color-buffer-bit))

(defn viewport [w h]
  (dosync (ref-set view-dim [w h]))
  (gl-viewport 0 0 w h))

(defmacro push-viewport [w h & body]
  `(let [[w# h#] @view-dim]
    (gl-viewport 0 0 ~w ~h)
    ~@body
    (gl-viewport 0 0 w# h#)))

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
  (let [[w h] @view-dim
        text-height (.. *text* (getBounds string) getHeight)]
    (.beginRendering *text* w h)
    (.draw *text* string (int (* x w)) (int (* y (- h text-height))))
    (.endRendering *text*)))


