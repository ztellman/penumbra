;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.core
  (:import (javax.media.opengl GLCanvas GL))
  (:import (javax.media.opengl.glu GLU))
  (:import (com.sun.opengl.util GLUT))
  (:import (java.lang.reflect Field)))

(def #^GL *gl* nil)
(def #^GLU *glu* (new GLU))
(def #^GLUT *glut* (new GLUT))

(def inside-begin-end false)
(def transform-matrix (atom nil))
(def view-bounds (ref [0 0 0 0]))

(defmacro bind-gl [#^javax.media.opengl.GLAutoDrawable drawable & body]
  `(binding [*gl* (.getGL ~drawable)]
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

(defn translate-keyword [k]
 (if (keyword? k)
   (let [gl (str "GL_" (.. (name k) (replace \- \_) (toUpperCase)))]
   `(. GL ~(symbol gl)))
   k))

(defmacro gl-import
  "Imports an OpenGL function, transforming all :keywords into GL_KEYWORDS"
  [import-from import-as]
  `(defmacro ~import-as [& args#]
    `(do
      (let [~'value# (. *gl* ~'~import-from ~@(map translate-keyword args#))]
        (if (and *check-errors* (not inside-begin-end)) (check-error))
        ~'value#))))

(defmacro glu-import [import-from import-as]
  `(defmacro ~import-as [& args#]
      `(. *glu* ~'~import-from ~@(map translate-keyword args#))))


(defmacro glut-import [import-from import-as]
  `(defmacro ~import-as [& args#]
      `(. *glut* ~'~import-from ~@(map translate-keyword args#))))

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

(gl-import glClear gl-clear)

(defn clear []
  (gl-clear :depth-buffer-bit)
  (gl-clear :color-buffer-bit))

(gl-import glViewport gl-viewport)

(defn viewport [x y w h]
  (dosync (ref-set view-bounds [x y w h]))
  (gl-viewport x y w h))

(defmacro with-viewport [[x y w h] & body]
  `(let [[x# y# w# h#] @view-bounds]
    (gl-viewport ~x ~y ~w ~h)
    ~@body
    (gl-viewport x# y# w# h#)))

;;;;;;;;;;;;;;;;;;;;;