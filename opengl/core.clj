;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.core
  (:use [clojure.contrib.def :only (defn-memo)])
  (:import (javax.media.opengl GL2))
  (:import (javax.media.opengl.awt GLCanvas))
  (:import (javax.media.opengl.glu.gl2 GLUgl2))
  (:import (com.sun.opengl.util.gl2 GLUT))
  (:import (java.lang.reflect Field)))

(def #^GL2 *gl* nil)
(def #^GLUgl2 *glu* (new GLUgl2))
(def #^GLUT *glut* (new GLUT))

(def inside-begin-end false)
(def transform-matrix (atom nil))
(def view-bounds (ref [0 0 0 0]))

(defmacro bind-gl [#^javax.media.opengl.GLAutoDrawable drawable & body]
  `(binding [*gl* (.. ~drawable getGL getGL2)]
    ~@body))

(defmacro push-matrix [& body]
  `(binding [transform-matrix (if inside-begin-end (atom @transform-matrix) transform-matrix)]
    (if (not inside-begin-end) (gl-push-matrix))
    ~@body
    (if (not inside-begin-end) (gl-pop-matrix))))

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
        (if (and *check-errors* (not inside-begin-end)) (check-error))
        ~'value#))))

(defmacro glu-import [import-from import-as]
  `(defmacro ~import-as [& args#]
      `(. *glu* ~'~import-from ~@(map translate-keyword-macro args#))))

(defmacro glut-import [import-from import-as]
  `(defmacro ~import-as [& args#]
      `(. *glut* ~'~import-from ~@(map translate-keyword-macro args#))))

;;;;;;;;;;;;;;;;;;;;;;

(gl-import glEnable enable)
(gl-import glDisable disable)

(gl-import glGetError gl-get-error)

(gl-import glMatrixMode gl-matrix-mode)
(gl-import glPushMatrix gl-push-matrix)
(gl-import glPopMatrix gl-pop-matrix)
(gl-import glLoadIdentity gl-load-identity-matrix)

(gl-import glBegin gl-begin)
(gl-import glEnd gl-end)

;;;;;;;;;;;;;;;;;;;;;;

(gl-import glClear gl-clear)

(defn clear []
  (gl-clear :depth-buffer-bit)
  (gl-clear :color-buffer-bit))

(gl-import glViewport gl-viewport)

(defn viewport
  ([w h] (viewport 0 0 w h))
  ([x y w h]
    (dosync (ref-set view-bounds [x y w h]))
    (gl-viewport x y w h)))

(defmacro with-viewport [[x y w h] & body]
  `(let [[x# y# w# h#] @view-bounds]
    (gl-viewport ~x ~y ~w ~h)
    ~@body
    (gl-viewport x# y# w# h#)))

;;;;;;;;;;;;;;;;;;;;;

(gl-import glOrtho gl-ortho)
(glu-import gluPerspective glu-perspective)

(defmacro with-projection [projection & body]
  `(do
    (gl-matrix-mode :projection) (gl-push-matrix) ~projection (gl-matrix-mode :modelview)
    ~@body
    (gl-matrix-mode :projection) (gl-pop-matrix) (gl-matrix-mode :modelview)))

(defn ortho-view
  "Create orthographic view, where distant objects don't get smaller."
  [left top right bottom near far]
  (gl-matrix-mode :projection)
  (gl-load-identity-matrix)
  (gl-ortho left right bottom top near far)
  (gl-matrix-mode :modelview))

(defn frustum-view [fovx aspect near far]
  "Create a standard perspective view."
  (gl-matrix-mode :projection)
  (gl-load-identity-matrix)
  (glu-perspective (double fovx) (double aspect) (double near) (double far))
  (gl-matrix-mode :modelview))

