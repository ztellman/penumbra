(ns penumbra.opengl.core
  (:use [clojure.contrib.def :only (defn-memo defmacro-)])
  (:import (javax.media.opengl GL2))
  (:import (javax.media.opengl.glu.gl2 GLUgl2))
  (:import (com.sun.opengl.util.gl2 GLUT))
  (:import (java.lang.reflect Field)))

(def #^GL2 *gl* nil)
(def #^GLUgl2 *glu* (new GLUgl2))
(def #^GLUT *glut* (new GLUT))

(def *inside-begin-end* false)
(def *transform-matrix* (atom nil))
(def *program* 0)
(def *texture-pool* nil)

;;;;;;;;;;;;;;;;;;;;;;

(def *check-errors* true) ;makes any OpenGL error throw an exception

(defn get-name
  "Takes the numeric value of a gl constant (i.e. GL_LINEAR), and gives the name"
  [enum-value]
  (if (= 0 enum-value)
    "NONE"
    (let [fields (seq (.. *gl* (getClass) (getFields)))]
      (.getName #^Field (some #(if (= enum-value (.get #^Field % *gl*)) % nil) fields)))))

(defn check-error []
  (let [error (.glGetError *gl*)]
    (if (not (zero? error))
      (throw (Exception. (str "OpenGL error: " (get-name error)))))))

(defn translate-keyword-macro [k]
 (if (keyword? k)
   (let [gl (str "GL_" (.. (name k) (replace \- \_) (toUpperCase)))]
    `(. GL2 ~(symbol gl)))
   k))

(defn-memo translate-keyword [k]
  (let [gl (str "GL_" (.. (name k) (replace \- \_) (toUpperCase)))]
    (eval `(. GL2 ~(symbol gl)))))

(defmacro gl-import
  "Imports an OpenGL function, transforming all :keywords into GL_KEYWORDS"
  [import-from import-as]
  `(defmacro ~import-as [& args#]
    `(do
      (let [~'value# (. #^GL2 *gl* ~'~import-from ~@(map translate-keyword-macro args#))]
        (if (and *check-errors* (not *inside-begin-end*)) (check-error))
        ~'value#))))

(defmacro gl-import-
  "Private version of gl-import"
  [name & decls]
  (list* `gl-import (with-meta name (assoc (meta name) :private true)) decls))

(defmacro glu-import [import-from import-as]
  `(defmacro ~import-as [& args#]
      `(. *glu* ~'~import-from ~@(map translate-keyword-macro args#))))

(defmacro glu-import-
  "Private version of glu-import"
  [name & decls]
  (list* `glu-import (with-meta name (assoc (meta name) :private true)) decls))

(defmacro glut-import [import-from import-as]
  `(defmacro ~import-as [& args#]
      `(. *glut* ~'~import-from ~@(map translate-keyword-macro args#))))

(defmacro glut-import-
  "Private version of glu-import"
  [name & decls]
  (list* `glut-import (with-meta name (assoc (meta name) :private true)) decls))

;;;;;;;;;;;;;;;;;;;;;;
