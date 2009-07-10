(ns penumbra.opengl.core)

(import '(javax.media.opengl GLCanvas GL)
        '(javax.media.opengl.glu GLU))

(set! *warn-on-reflection* true)

(def #^GL *gl* nil)
(def #^GLU *glu* (new GLU))

(defmacro bind-gl [#^javax.media.opengl.GLAutoDrawable drawable & body]
  `(binding [*gl* (.getGL ~drawable)]
    ~@body))

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

;;;;;;;;;;;;;;;;;;;;;;

(gl-import glEnable enable)
(gl-import glDisable disable)

(gl-import glMatrixMode gl-matrix-mode)

(gl-import glBegin gl-begin)
(gl-import glEnd gl-end)

(gl-import glPushMatrix gl-push-matrix)
(gl-import glPopMatrix gl-pop-matrix)


