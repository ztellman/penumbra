(ns penumbra.opengl)

(import '(javax.media.opengl GLCanvas GL)
        '(javax.media.opengl.glu GLU)
        '(java.awt Font)
        '(com.sun.opengl.util.j2d TextRenderer))

(use 'penumbra.matrix)

(set! *warn-on-reflection* true)

(def #^GL *gl* nil)
(def #^GLU *glu* (new GLU))
(def transform-stack (ref [(identity-matrix)]))
(def inside-begin-end (ref false))

(defmacro bind-gl [#^javax.media.opengl.GLAutoDrawable drawable & args]
  `(binding [*gl* (.getGL ~drawable)]
    ~@args))

;;;;;;;;;;;;;;;;;;;;;;

(defn prepend [text sym] (symbol (format "%s-%s" text (name sym))))

(defmacro gl-facade-import
  "Takes an OpenGL function and turns it into two macros:
    - a macro which behaves differently if we're inside a glBegin/End clause (glVertex3d -> vertex)
    - a macro which directly calls the OpenGL function (glVertex3d -> gl-vertex)"
  [import-from import-as]
  (let [facade-fn (prepend "facade" import-as)
       direct-fn (prepend "gl" import-as)]
    `(do
      (defmacro ~import-as [& a#]
        `(if @inside-begin-end
          (~'~facade-fn ~@a#)
          (. *gl* ~'~import-from ~@a#)))
      (defmacro ~direct-fn [& b#]
        `(. *gl* ~'~import-from ~@b#)))))

(defmacro glu-import [import-from import-as]
  `(defmacro ~import-as [& args#]
      `(. *glu* ~'~import-from ~@args#)))

(defn translate-keyword [k]
 (if (keyword? k)
   (let [gl (str "GL_" (.. (name k) (replace \- \_) (toUpperCase)))]
   `(. GL ~(symbol gl)))
   k))

(defmacro gl-import
  "Imports an OpenGL function, transforming all :keywords into GL_KEYWORDS"
  [import-from import-as]
  `(defmacro ~import-as [& args#]
    `(. *gl* ~'~import-from ~@(map translate-keyword args#))))

(gl-facade-import glVertex3d vertex)
(gl-facade-import glNormal3d normal)
(gl-facade-import glRotated rotate)
(gl-facade-import glTranslated translate)
(gl-facade-import glScaled scale)
(gl-facade-import glLoadIdentity load-identity)

(gl-import glColor3d color)

(gl-import glBegin gl-begin)
(gl-import glEnd gl-end)

(gl-import glPushMatrix gl-push-matrix)
(gl-import glPopMatrix gl-pop-matrix)
(gl-import glMatrixMode gl-matrix-mode)

(gl-import glOrtho gl-ortho)
(glu-import gluPerspective glu-perspective)

(gl-import glEnable enable)
(gl-import glDisable disable)
(gl-import glCullFace gl-cull-face)
(gl-import glPolygonMode gl-polygon-mode)
(gl-import glHint gl-hint)
(gl-import glClear gl-clear)

(gl-import glCallList gl-call-list)
(gl-import glGenLists gl-gen-lists)
(gl-import glNewList gl-new-list)
(gl-import glEndList gl-end-list)
(gl-import glDeleteLists gl-delete-lists)
(gl-import glIsList gl-is-list)

(gl-import glLightfv set-light)
(gl-import glMaterialfv set-material)
(gl-import glBlendFunc gl-blend-func)
(gl-import glShadeModel shade-model)

;;;;;;;;;;;;;;;;;;;;;;;;;
;Render functions

(defmacro facade-transform
  "Forwards the transformed vector from fn to the OpenGL function fn represents."
  [fn transform-fn]
  (let [facade-fn (prepend "facade" fn)
        direct-fn (prepend "gl" fn)]
    `(defn ~facade-fn [x# y# z#]
      (let [[xp# yp# zp# wp#] (apply-matrix (~transform-fn (peek @transform-stack)) [x# y# z# 1])]
        (~direct-fn xp# yp# zp#)))))

(defn undo-translation [matrix] (vec (concat (subvec matrix 0 12) [0 0 0 0]))) ;we don't want to translate normals

(facade-transform vertex identity)
(facade-transform normal undo-translation)

(defn apply-transform
  "Pops off the head of the transform stack, multiplies it by the matrix, and pushes it back on"
  [matrix transform-fn]
  (dosync
    (ref-set transform-stack (conj (pop @transform-stack) (transform-fn (peek @transform-stack) matrix)))))

(defmacro facade-multiply
  "Applies a transform to transform-stack rather than the OpenGL modelview matrix."
  ([fn matrix-fn] `(facade-multiply ~fn ~matrix-fn mult-matrix))
  ([fn matrix-fn transform-fn]
  (let [facade-fn (prepend "facade" fn)]
    `(defmacro ~facade-fn [& args#]
      `(apply-transform (~'~matrix-fn ~@args#) ~'~transform-fn)))))

(facade-multiply rotate rotation-matrix)
(facade-multiply scale scaling-matrix)
(facade-multiply translate translation-matrix)
(facade-multiply load-identity identity-matrix #(%2)) ;Note: this only resets transformations local to the begin/end clause

(defmacro defn-draw
  "Creates a macro called draw-'type' which redirects vertex and transform calls through appropriate facades."
  [primitive-type]
  `(defmacro ~(symbol (str "draw-" (name primitive-type))) [& args#]
    `(do
      (dosync
        (ref-set transform-stack [(identity-matrix)])
        (ref-set inside-begin-end true))
      (gl-begin ~'~(translate-keyword primitive-type))
      ~@args#
      (gl-end)
      (dosync
        (ref-set inside-begin-end false)))))

(defn-draw :quads)
(defn-draw :line-strip)
(defn-draw :lines)
(defn-draw :triangle-strip)
(defn-draw :triangle-fan)
(defn-draw :quad-strip)
(defn-draw :triangles)

(defn clear []
  (gl-clear :depth-buffer-bit)
  (gl-clear :color-buffer-bit))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;Various utility functions

(defmacro push-matrix [& args]
  `(if @inside-begin-end
    (do
      (dosync (ref-set transform-stack (conj (deref transform-stack) (peek (deref transform-stack)))))
      ~@args
      (dosync (ref-set transform-stack (pop (deref transform-stack)))))
    (do
      (gl-push-matrix)
      ~@args
      (gl-pop-matrix))))

(defmacro set-display-list [list-ref & args]
  "Points list-ref to a new list, and deletes the list it was previous pointing to."
  `(let [list# (gl-gen-lists 1)]
    (do
      (gl-new-list list# :compile)
      ~@args
      (gl-end-list))
    (if (is-display-list ~list-ref) (delete-display-list ~list-ref))
    (dosync (ref-set ~list-ref list#))))

(defn is-display-list [list-ref]
  (and
    (not (nil? @list-ref))
    (gl-is-list @list-ref)))

(defn delete-display-list [list-ref]
  (gl-delete-lists @list-ref 1))

(defn call-display-list [list-ref]
  (gl-call-list @list-ref))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Render state

(defn enable-anti-aliasing []
  (enable :point-smooth)
  (enable :line-smooth)
  (enable :polygon-smooth)
  (gl-hint :point-smooth-hint :nicest)
  (gl-hint :line-smooth-hint :nicest)
  (gl-hint :polygon-smooth-hint :nicest)
  (enable :blend)
  (gl-blend-func :src-alpha :one-minus-src-alpha))

(defmacro set-light-position [num [x y z w]]
  (let [light# (keyword (str "light" num))]
    `(do
      (enable :lighting)
      (enable ~light#)
      (set-light ~light# :position (float-array 4 [~x ~y ~z ~w]) 0))))

(defn material [r g b a]
  (set-material :front-and-back :ambient-and-diffuse (float-array 4 [r g b a]) 0))

(defn draw-solid [] (gl-polygon-mode :front-and-back :fill))
(defn draw-wireframe [] (gl-polygon-mode :front-and-back :line))
(defn draw-point-cloud [] (gl-polygon-mode :front-and-back :point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;View initialization

(defn ortho-view
  "Create orthographic view, where distant objects don't get smaller."
  [left right bottom top near far]
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



