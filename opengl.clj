(ns vellum.opengl)

(import '(javax.media.opengl GLCanvas GL)
        '(javax.media.opengl.glu GLU)
        '(java.awt Font)
        '(com.sun.opengl.util.j2d TextRenderer))

(use 'vellum.matrix)

(set! *warn-on-reflection* true)

(def #^GL *gl* nil)
(def #^GLU *glu* (new GLU))
(def *transform-stack* (ref [(identity-matrix)]))
(def *inside-begin-end* (ref false))

(defmacro bind-gl [#^javax.media.opengl.GLAutoDrawable drawable & args]
  `(binding [*gl* (.getGL ~drawable)]
    ~@args))

;;;;;;;;;;;;;;;;;;;;;;

(defn prepend [text sym] (symbol (format "%s-%s" text (name sym))))

(defmacro gl-import [import-from import-as]
  "Creates a macro which calls an OpenGL function"
  `(defmacro ~import-as [& args#]
    `(. *gl* ~'~import-from ~@args#)))

(defmacro gl-facade-import
  "Takes an OpenGL function and turns it into two macros:
    - a macro which behaves differently if we're inside a glBegin/End clause (glVertex3d -> vertex)
    - a macro which directly calls the OpenGL function (glVertex3d -> gl-vertex)"
  [import-from import-as]
  (let [facade-fn (prepend "facade" import-as)
       direct-fn (prepend "gl" import-as)]
    `(do
      (defmacro ~import-as [& a#]
        `(if @*inside-begin-end*
          (~'~facade-fn ~@a#)
          (. *gl* ~'~import-from ~@a#)))
      (defmacro ~direct-fn [& b#]
        `(. *gl* ~'~import-from ~@b#)))))

(defmacro glu-import [import-from import-as]
  `(defmacro ~import-as [& args#]
      `(. *glu* ~'~import-from ~@args#)))

(defmacro gl-value [import-from import-as]
  `(def ~import-as (. GL ~import-from)))

;create macro which will redirect to a transform macro when inside glBegin/glEnd
(gl-facade-import glVertex3d vertex)
(gl-facade-import glNormal3d normal)
(gl-facade-import glRotated rotate)
(gl-facade-import glTranslated translate)
(gl-facade-import glScaled scale)
(gl-facade-import glLoadIdentity load-identity)

(gl-import glColor3d color)

;straightforward imports
(gl-import glBegin gl-begin)
(gl-import glEnd gl-end)
(gl-value GL_QUADS quads)
(gl-value GL_TRIANGLE_STRIP triangle-strip)
(gl-value GL_LINES lines)
(gl-value GL_LINE_STRIP line-strip)
(gl-value GL_TRIANGLE_FAN triangle-fan)
(gl-value GL_QUAD_STRIP quad-strip)
(gl-value GL_TRIANGLES triangles)

(gl-import glPushMatrix gl-push-matrix)
(gl-import glPopMatrix gl-pop-matrix)
(gl-import glMatrixMode gl-matrix-mode)
(gl-value GL_PROJECTION projection)
(gl-value GL_MODELVIEW modelview)

(gl-import glOrtho gl-ortho)
(glu-import gluPerspective glu-perspective)

(gl-import glEnable gl-enable)
(gl-import glDisable gl-disable)
(gl-value GL_DEPTH_TEST gl-depth-test)
(gl-value GL_CULL_FACE gl-cull-face)
(gl-value GL_AUTO_NORMAL gl-auto-normal)

(gl-import glCullFace gl-cull-face-func)
(gl-value GL_FRONT front)
(gl-value GL_BACK back)
(gl-value GL_FRONT_AND_BACK front-and-back)

(gl-import glPolygonMode gl-polygon-mode)
(gl-value GL_LINE_SMOOTH smooth-lines)
(gl-value GL_POINT_SMOOTH smooth-points)
(gl-value GL_POLYGON_SMOOTH smooth-polygons)
(gl-value GL_LINE wireframe)
(gl-value GL_POINT point-cloud)
(gl-value GL_FILL solid)

(gl-import glHint gl-hint)
(gl-value GL_LINE_SMOOTH_HINT line-quality)
(gl-value GL_POINT_SMOOTH_HINT point-quality)
(gl-value GL_POLYGON_SMOOTH_HINT polygon-quality)
(gl-value GL_NICEST high-quality)
(gl-value GL_FASTEST low-quality)
(gl-value GL_DONT_CARE default-quality)

(gl-import glClear gl-clear)

(gl-import glCallList gl-call-list)
(gl-import glGenLists gl-gen-lists)
(gl-import glNewList gl-new-list)
(gl-import glEndList gl-end-list)
(gl-import glDeleteLists gl-delete-lists)
(gl-import glIsList gl-is-list)
(gl-value GL_COMPILE compile-list)

(gl-import glLightfv set-light)
(gl-value GL_LIGHTING lighting)
(gl-value GL_POSITION light-position)

(gl-import glMaterialfv set-material)
(gl-value GL_AMBIENT_AND_DIFFUSE ambient-and-diffuse)

(gl-import glBlendFunc gl-blend-func)
(gl-value GL_BLEND gl-blend)
(gl-value GL_SRC_ALPHA gl-src-alpha)
(gl-value GL_ONE_MINUS_SRC_ALPHA gl-one-minus-src-alpha)

(gl-import glShadeModel shade-model)
(gl-value GL_FLAT flat-shading)
(gl-value GL_SMOOTH smooth-shading)

(gl-value GL_DEPTH_BUFFER_BIT depth-buffer)
(gl-value GL_COLOR_BUFFER_BIT color-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;
;Render functions

(defmacro facade-transform
  "Forwards the transformed vector from fn to the OpenGL function fn represents."
  [fn]
  (let [facade-fn (prepend "facade" fn)
        direct-fn (prepend "gl" fn)]
    `(defn ~facade-fn [x# y# z#]
      (let [[xp# yp# zp# wp#] (apply-matrix (peek @*transform-stack*) [x# y# z# 1])]
        (~direct-fn xp# yp# zp#)))))

(facade-transform vertex)
(facade-transform normal)

(defn apply-transform
  "Pops off the head of the transform stack, multiplies it by the matrix, and pushes it back on"
  [matrix]
  (dosync
    (ref-set *transform-stack* (conj (pop @*transform-stack*) (mult-matrix (peek @*transform-stack*) matrix)))))

(defmacro facade-multiply
  "Applies a transform to *transform-stack* rather than the OpenGL modelview matrix."
  [fn matrix-fn]
  (let [facade-fn (prepend "facade" fn)]
    `(defmacro ~facade-fn [& args#]
      `(apply-transform (~'~matrix-fn ~@args#)))))

(facade-multiply rotate rotation-matrix)
(facade-multiply scale scaling-matrix)
(facade-multiply translate translation-matrix)
(facade-multiply load-identity identity-matrix) ;Note: this only resets transformations local to the begin/end clause

(defmacro defn-draw
  "Creates a macro called draw-'type' which redirects vertex and transform calls through appropriate facades."
  [type]
  `(defmacro ~(prepend "draw" type) [& args#]
    `(do
      (dosync
        (ref-set *transform-stack* [(identity-matrix)])
        (ref-set *inside-begin-end* true))
      (gl-begin ~'~type)
      ~@args#
      (gl-end)
      (dosync
        (ref-set *inside-begin-end* false)))))

(defn-draw quads)
(defn-draw line-strip)
(defn-draw lines)
(defn-draw triangle-strip)
(defn-draw triangle-fan)
(defn-draw quad-strip)
(defn-draw triangles)

(defn clear []
  (gl-clear (+ depth-buffer color-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;Various utility functions

(defmacro push-matrix [& args]
  `(if @*inside-begin-end*
    (do
      (dosync (ref-set *transform-stack* (conj (deref *transform-stack*) (peek (deref *transform-stack*)))))
      ~@args
      (dosync (ref-set *transform-stack* (pop (deref *transform-stack*)))))
    (do
      (gl-push-matrix)
      ~@args
      (gl-pop-matrix))))

(defmacro set-list [list-ref & args]
  "Points list-ref to a new list, and deletes the list it was previous pointing to."
  `(let [list# (gl-gen-lists 1)]
    (do
      (gl-new-list list# compile-list)
      ~@args
      (gl-end-list))
    (if (is-list ~list-ref) (delete-list ~list-ref))
    (dosync (ref-set ~list-ref list#))))

(defn is-list [list-ref]
  (and
    (not (nil? @list-ref))
    (gl-is-list @list-ref)))

(defn delete-list [list-ref]
  (gl-delete-lists @list-ref 1))

(defn call-list [list-ref]
  (gl-call-list @list-ref))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Enable/Disable

(defmacro def-toggle
  [import-from import-as]
  (let [enable# (prepend "enable" import-as)
        disable# (prepend "disable" import-as)]
    `(do
      (defmacro ~enable# [] `(gl-enable ~'~import-from))
      (defmacro ~disable# [] `(gl-disable ~'~import-from)))))

(def-toggle gl-depth-test depth-test)
(def-toggle gl-cull-face cull-face)
(def-toggle gl-auto-normal auto-normals)
(def-toggle gl-blend alpha-blending)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Render state

(defn cull-back [] (gl-cull-face-func back))
(defn cull-front [] (gl-cull-face-func front))
(defn cull-front-and-back [] (gl-cull-face-func front-and-back))

(defn set-anti-aliasing [enable]
  (let [fn (if enable #(gl-enable %) #(gl-disable %))]
    (fn smooth-points)
    (fn smooth-lines)
    (fn smooth-polygons)))

(defn set-anti-aliasing-quality [quality]
  (gl-hint point-quality quality)
  (gl-hint line-quality quality)
  (gl-hint polygon-quality quality))

(defn enable-anti-aliasing []
  (set-anti-aliasing true)
  (set-anti-aliasing-quality high-quality)
  (gl-enable gl-blend)
  (gl-blend-func gl-src-alpha gl-one-minus-src-alpha))

(defmacro setup-light [num [x y z]]
  `(let [light# (. GL ~(symbol (format "GL_LIGHT%d" num)))]
    (do
      (gl-enable lighting)
      (gl-enable light#)
      (set-light light# light-position (float-array 4 [~x ~y ~z 1]) 0))))

(defn material [r g b a]
  (set-material front-and-back ambient-and-diffuse (float-array 4 [r g b a]) 0))

(defn render-mode [mode]
  (gl-polygon-mode front-and-back mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;View initialization

(defn ortho-view
  "Create orthographic view, where distant objects don't get smaller."
  [left right bottom top near far]
  (gl-matrix-mode projection)
  (load-identity)
  (gl-ortho left right bottom top near far)
  (gl-matrix-mode modelview))

(defn frustum-view [fovx aspect near far]
  "Create a standard perspective view."
  (gl-matrix-mode projection)
  (load-identity)
  (glu-perspective fovx aspect near far)
  (gl-matrix-mode modelview))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;Text rendering

(def view-dimensions (ref [0 0]))
(def #^TextRenderer *text* (TextRenderer. (Font. "Tahoma" java.awt.Font/PLAIN 20) true true))

(defn set-dimensions [w h]
  (dosync (ref-set view-dimensions [w h])))

(defn write
  "writes string at normalized coordinates (x,y)"
  [string x y]
  (let [[w h] @view-dimensions
        text-height (.. *text* (getBounds string) getHeight)]
    (.beginRendering *text* w h)
    (.draw *text* string (int (* x w)) (int (* y (- h text-height))))
    (.endRendering *text*)))



