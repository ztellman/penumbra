(ns vellum.opengl)

(import '(javax.media.opengl GLCanvas GL)
        '(javax.media.opengl.glu GLU)
        '(java.awt Font)
        '(com.sun.opengl.util.j2d TextRenderer))

(use 'vellum.matrix)

(set! *warn-on-reflection* true)

(def #^GL *gl* nil)
(def #^GLU *glu* (new GLU))

(def *matrix* (ref (identity-matrix)))
(def *redirect* (ref false))

(defmacro bind-gl [#^javax.media.opengl.GLAutoDrawable drawable & args]
  `(binding [*gl* (.getGL ~drawable)]
    ~@args))

;;;;;;;;;;;;;;;;;;;;;;

(defn prepend [text sym] (symbol (format "%s-%s" text (name sym))))

(defmacro gl-import [import-from import-as]
  `(defmacro ~import-as [& args#]
    `(. *gl* ~'~import-from ~@args#)))

(defmacro gl-facade-import [import-from import-as]
  (let [facade-fn (prepend "facade" import-as)
       direct-fn (prepend "gl" import-as)]
    `(do
      (defmacro ~import-as [& a#]
        `(if @*redirect*
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
(gl-facade-import glColor3d color)
(gl-facade-import glRotated rotate)
(gl-facade-import glTranslated translate)
(gl-facade-import glScalef scale)

;straightforward imports
(gl-import glBegin gl-begin)
(gl-import glEnd gl-end)

(gl-import glLoadIdentity load-identity)
(gl-import glPushMatrix gl-push-matrix)
(gl-import glPopMatrix gl-pop-matrix)
(gl-import glMatrixMode gl-matrix-mode)

(gl-import glOrtho gl-ortho)
(glu-import gluPerspective glu-perspective)

(gl-import glClear gl-clear)

(gl-import glCallList gl-call-list)
(gl-import glGenLists gl-gen-lists)
(gl-import glNewList gl-new-list)
(gl-import glEndList gl-end-list)
(gl-import glDeleteList gl-delete-list)
(gl-import glIsList gl-is-list)

;predefined values
(gl-value GL_QUADS quads)
(gl-value GL_TRIANGLE_STRIP triangle-strip)
(gl-value GL_LINES lines)
(gl-value GL_LINE_STRIP line-strip)
(gl-value GL_PROJECTION projection)
(gl-value GL_MODELVIEW modelview)
(gl-value GL_DEPTH_BUFFER_BIT depth-buffer)
(gl-value GL_COLOR_BUFFER_BIT color-buffer)
(gl-value GL_COMPILE compile-list)

;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro facade-transform
  "Forwards a vector transformed by *matrix* from fn to the OpenGL function fn represents."
  [fn]
  (let [facade-fn (prepend "facade" fn)
        direct-fn (prepend "gl" fn)]
    `(defn ~facade-fn [x# y# z#]
      (let [[xp# yp# zp# wp#] (apply-matrix @*matrix* [x# y# z# 1])]
        (~direct-fn xp# yp# zp#)))))

(facade-transform vertex)
(facade-transform normal)

(defmacro facade-multiply
  "Applies a transform to *matrix* rather than the OpenGL modelview matrix."
  [fn matrix-fn]
  (let [facade-fn (prepend "facade" fn)]
    `(defmacro ~facade-fn [& args#]
      `(dosync (ref-set *matrix* (mult-matrix @*matrix* (~'~matrix-fn ~@args#)))))))

(facade-multiply rotate rotation-matrix)
(facade-multiply scale scaling-matrix)
(facade-multiply translate translation-matrix)

(defmacro defn-draw
  "Creates a macro called draw-'type' which redirects vertex and transform calls through appropriate facades."
  [type]
  `(defmacro ~(prepend "draw" type) [& args#]
    `(do
      (dosync
        (ref-set *matrix* (identity-matrix))
        (ref-set *redirect* true))
      (gl-begin ~'~type)
      ~@args#
      (gl-end)
      (dosync
        (ref-set *redirect* false)))))

(defn-draw quads)
(defn-draw line-strip)
(defn-draw lines)
(defn-draw triangle-strip)

;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro push-matrix [& args]
  `(do
    (gl-push-matrix)
    ~@args
    (gl-pop-matrix)))

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
  (gl-delete-list @list-ref 1))

(defn call-list [list-ref]
  (gl-call-list @list-ref))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn clear []
  (gl-clear (+ depth-buffer color-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

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



