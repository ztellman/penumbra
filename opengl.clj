(ns vellum.opengl)

(import '(javax.media.opengl GLCanvas GL)
        '(javax.media.opengl.glu GLU))

(set! *warn-on-reflection* true)

(def #^GL *gl* nil)
(def #^GLU *glu* (new GLU))

(defmacro gl-import [import-from import-as]
  `(defmacro ~import-as [& args#]
      `(. *gl* ~'~import-from ~@args#)))

(defmacro glu-import [import-from import-as]
  `(defmacro ~import-as [& args#]
      `(. *glu* ~'~import-from ~@args#)))

(defmacro gl-value [import-from import-as]
  `(def ~import-as (. GL ~import-from)))

;renamed functions
(gl-import glVertex3d vertex)
(gl-import glColor3d color)
(gl-import glRotated rotate)
(gl-import glTranslated translate)
(gl-import glScalef scale)
(gl-import glLoadIdentity load-identity)

;gl-style functions, which will likely be wrapped in some other function
(gl-import glBegin gl-begin)
(gl-import glEnd gl-end)
(gl-import glPushMatrix gl-push-matrix)
(gl-import glPopMatrix gl-pop-matrix)
(gl-import glMatrixMode gl-matrix-mode)
(gl-import glFrustrum gl-frustrum)
(gl-import glOrtho gl-ortho)
(gl-import glClear gl-clear)
(glu-import gluPerspective glu-perspective)

;predefined values
(gl-value GL_QUADS quads)
(gl-value GL_TRIANGLE_STRIP triangle-strip)
(gl-value GL_LINES lines)
(gl-value GL_LINE_STRIP line-strip)
(gl-value GL_PROJECTION projection)
(gl-value GL_MODELVIEW modelview)
(gl-value GL_DEPTH_BUFFER_BIT depth-buffer)
(gl-value GL_COLOR_BUFFER_BIT color-buffer)


(defmacro bind-gl [#^javax.media.opengl.GLAutoDrawable drawable & args]
  `(binding [*gl* (.getGL ~drawable)]
    ~@args))

(defmacro push-matrix [& args]
  `(do
    (gl-push-matrix)
    ~@args
    (gl-pop-matrix)))

(defmacro defn-draw
  "transforms (defn-draw line-strip) into a macro called 'draw-line-strip'"
  [type]
  `(defmacro ~(symbol (format "draw-%s" (name type))) [& args#]
    `(do
      (gl-begin ~'~type)
      ~@args#
      (gl-end))))

(defn-draw quads)
(defn-draw line-strip)
(defn-draw lines)
(defn-draw triangle-strip)

(defn clear []
  (gl-clear (+ depth-buffer color-buffer)))

(defn ortho-view [left right bottom top near far]
  (gl-matrix-mode projection)
  (load-identity)
  (gl-ortho left right bottom top near far)
  (gl-matrix-mode modelview))

(defn frustum-view [fovx aspect near far]
  (gl-matrix-mode projection)
  (load-identity)
  (glu-perspective fovx aspect near far)
  (gl-matrix-mode modelview))
