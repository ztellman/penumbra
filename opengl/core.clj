(ns penumbra.opengl.core)

(import '(javax.media.opengl GLCanvas GL)
        '(javax.media.opengl.glu GLU))

(set! *warn-on-reflection* true)
(def inside-begin-end false)

(def #^GL *gl* nil)
(def #^GLU *glu* (new GLU))

(defmacro bind-gl [#^javax.media.opengl.GLAutoDrawable drawable & body]
  `(binding [*gl* (.getGL ~drawable)]
    ~@body))

(defmacro glu-import [import-from import-as]
  `(defmacro ~import-as [& args#]
      `(. *glu* ~'~import-from ~@(map translate-keyword args#))))

(defn translate-keyword [k]
 (if (keyword? k)
   (let [gl (str "GL_" (.. (name k) (replace \- \_) (toUpperCase)))]
   `(. GL ~(symbol gl)))
   k))

(defmacro gl-import    ;TODO: add call to glGetError after each call for debug purposes
  "Imports an OpenGL function, transforming all :keywords into GL_KEYWORDS"
  [import-from import-as]
  `(defmacro ~import-as [& args#]
    `(. *gl* ~'~import-from ~@(map translate-keyword args#))))

;;;;;;;;;;;;;;;;;;;;;;

(gl-import glEnable enable)
(gl-import glDisable disable)
(gl-import glGetError gl-get-error)

(gl-import glMatrixMode gl-matrix-mode)
(gl-import glPushMatrix gl-push-matrix)
(gl-import glPopMatrix gl-pop-matrix)

(gl-import glBegin gl-begin)
(gl-import glEnd gl-end)

;;;;;;;;;;;;;;;;;;;;;;

(defn get-name
  "Takes the numeric value of a gl constant (i.e. GL_LINEAR), and gives the name"
  [enum-value]
  (if (= 0 enum-value)
    "NONE"
    (let [fields (seq (.. *gl* (getClass) (getFields)))]
      (.getName (some #(if (= enum-value (.get % *gl*)) % nil) fields)))))