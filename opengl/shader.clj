;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.shader
  (:use [clojure.contrib.def :only (defmacro-)])
  (:use [penumbra.opengl core translate])
  (:import (java.nio ByteBuffer IntBuffer FloatBuffer))
  (:import (com.sun.opengl.util BufferUtil)))

;;;;;;;;;;;;;;;;;;

(gl-import glCreateShader gl-create-shader)
(gl-import glShaderSource gl-shader-source)
(gl-import glCompileShader gl-compile-shader)
(gl-import glAttachShader gl-attach-shader)
(gl-import glGetShaderInfoLog gl-get-shader-info-log)
(gl-import glGetShaderiv gl-get-shader)

(gl-import glCreateProgram gl-create-program)
(gl-import glLinkProgram gl-link-program)
(gl-import glValidateProgram gl-validate-program)
(gl-import glUseProgram gl-use-program)
(gl-import glGetProgramInfoLog gl-get-program-info-log)
(gl-import glGetProgramiv gl-get-program)

(gl-import glUniform1iARB uniform-1i)
(gl-import glUniform2iARB uniform-2i)
(gl-import glUniform3iARB uniform-3i)
(gl-import glUniform4iARB uniform-4i)

(gl-import glUniform1fARB uniform-1f)
(gl-import glUniform2fARB uniform-2f)
(gl-import glUniform3fARB uniform-3f)
(gl-import glUniform4fARB uniform-4f)

(gl-import glGetUniformLocation get-uniform-location)

(defstruct gpu-program :vertex :fragment :program)

;;;;;;;;;;;;;;;;;;;;
;OpenGL shader functions

(def *verbose* true)

(defmacro- gl-query-status
  [query-fn setting fn-name]
  `(defn- ~fn-name [param#]
    (let [a# (int-array 1)]
      (~query-fn param# ~setting a# 0)
      (not (zero? (nth a# 0))))))

(gl-query-status gl-get-shader :compile-status shader-compiled?)
(gl-query-status gl-get-program :link-status program-linked?)
(gl-query-status gl-get-program :validate-status program-valid?)

(defn- get-shader-log [shader]
  (let [buf (make-array Byte/TYPE 4096)]
    (gl-get-shader-info-log shader 4096 (int-array 1) 0 buf 0)
    (.trim #^String (apply str (map #(char %) buf)))))

(defn- load-source [shader source]
  (let [a (make-array String 1)]
    (aset a 0 source)
    (gl-shader-source shader 1 a nil))
  (gl-compile-shader shader)
  (if *verbose*
    (println source))
  (if (shader-compiled? shader)
    (if *verbose* (println "*** Compiled ***"))
    (throw (Exception. (str "*** Error compiling shader: " (get-shader-log shader))))))

(defn- get-program-log [program]
  (let [buf (make-array Byte/TYPE 4096)]
    (gl-get-program-info-log program 4096 (int-array 1) 0 buf 0)
    (.trim (apply str (map #(char %) buf)))))

(defn create-program-from-source
  "Takes translated source (in GLSL, not s-expressions) for two shaders, and combines them into a program"
  [vertex-source fragment-source]
  (let [vertex-shader (gl-create-shader :vertex-shader)
        fragment-shader (gl-create-shader :fragment-shader)
        program (gl-create-program)]
    (load-source vertex-shader vertex-source)
    (load-source fragment-shader fragment-source)
    (gl-attach-shader program vertex-shader)
    (gl-attach-shader program fragment-shader)
    (gl-link-program program)
    (if (not (program-linked? program))
      (throw (Exception. (str "*** Error linking program: " (get-program-log program)))))
    (gl-validate-program program)
    (if (not (program-valid? program))
      (throw (Exception. (str "*** Error validating program: "(get-program-log program)))))
    (if *verbose* (println "*** Linked ***"))
    (struct-map gpu-program :vertex vertex-shader :fragment fragment-shader :program program)))

(defn bind-program
  [program]
  (gl-use-program (if (nil? program) 0 (:program program))))
                     
(defn create-program
  "Creates a program from s-exprssions.  Declarations are specified first, and shared between both shaders."
  ([declarations vertex fragment]
    (create-program "" declarations vertex fragment))
  ([extensions declarations vertex fragment]
    (let [vertex-source (translate-shader declarations vertex)
          fragment-source (translate-shader (filter #(not= 'attribute (first %)) declarations) fragment)]
      (create-program-from-source
        (str extensions "\n" vertex-source)
        (str extensions "\n" fragment-source)))))

;;;;;;;;;;;;;;;;;;;;

(def *program* 0)

(defn- or= [cmp & args] (some #(= cmp %) args))

(defmacro with-program [program & body]
  `(binding [*program* (:program ~program)]
    (bind-program ~program)
    ~@body))

(defn uniform-location [n]
  (get-uniform-location *program* n))

'(defn param [variable & args]
  (if (and (number? (first args)) (= 1 (count args)))
    (uniform-1i (uni-loc *program* (name variable)) (:id (first args)))
    (let [type  (if (or= (class (second args)) Integer Integer/TYPE) "i" "f")
          count (count args)]
      (eval
        `(~(symbol (str "uniform-" count type))
          (uni-loc ~*program* ~(name variable))
          ~@args)))))
