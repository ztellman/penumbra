;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.shader
  (:use [clojure.contrib.def :only (defmacro- defvar)])
  (:use [clojure.contrib.seq-utils :only (indexed)])
  (:use [penumbra.opengl.core])
  (:use [penumbra.glsl.core])
  (:use [penumbra.translate.core])
  (:import (java.nio ByteBuffer IntBuffer FloatBuffer)))

;;;;;;;;;;;;;;;;;;

(gl-import- glCreateShader gl-create-shader)
(gl-import- glShaderSource gl-shader-source)
(gl-import- glCompileShader gl-compile-shader)
(gl-import- glAttachShader gl-attach-shader)
(gl-import- glGetShaderInfoLog gl-get-shader-info-log)
(gl-import- glGetShader gl-get-shader)

(gl-import- glCreateProgram gl-create-program)
(gl-import- glLinkProgram gl-link-program)
(gl-import- glValidateProgram gl-validate-program)
(gl-import- glGetProgramInfoLog gl-get-program-info-log)
(gl-import- glGetProgram gl-get-program)

(gl-import glUniform1i uniform-1i)
(gl-import glUniform2i uniform-2i)
(gl-import glUniform3i uniform-3i)
(gl-import glUniform4i uniform-4i)

(gl-import glUniform1f uniform-1f)0
(gl-import glUniform2f uniform-2f)
(gl-import glUniform3f uniform-3f)
(gl-import glUniform4f uniform-4f)

(defstruct program-struct :vertex :fragment :program :uniforms)

;;;;;;;;;;;;;;;;;;;;
;OpenGL shader functions

(defvar *verbose* true
  "Full feedback on shader compilation.")

(defmacro- gl-query-status
  [query-fn setting fn-name]
  `(defn- ~fn-name [param#]
    (let [a# (int-array 1)]
      (~query-fn param# ~setting (IntBuffer/wrap a#))
      (not (zero? (first a#))))))

(gl-query-status gl-get-shader :compile-status shader-compiled?)
(gl-query-status gl-get-program :link-status program-linked?)
(gl-query-status gl-get-program :validate-status program-valid?)

(defn- get-shader-log [shader]
  (let [buf (make-array Byte/TYPE 4096)]
    (gl-get-shader-info-log shader 4096 buf)
    (.trim #^String (apply str (map #(char %) buf)))))

(defn- replace-string [regex replacement s]
  (.replaceAll (re-matcher regex s) replacement))

(defn- cleanup-braces [source]
  (->> source
       (replace-string #"\n\s[\s]+\{" " {")
       (replace-string #"\}[;\n]?[\s]+\}" "}}")))

(defn- load-source [shader source]
  (let [source (-> source cleanup-braces cleanup-braces)
        source-buf (ByteBuffer/wrap (.getBytes source))]
    (gl-shader-source shader source-buf)
    (gl-compile-shader shader)
    (if *verbose*
      (println (indent source)))
    (if (shader-compiled? shader)
      (if *verbose* (println "Compiled.\n"))
      (throw (Exception. (str "*** Error compiling shader: " (get-shader-log shader)))))))

(defn- get-program-log [program]
  (let [buf (byte-array 4096)]
    (gl-get-program-info-log program 4096 (ByteBuffer/wrap buf))
    (.trim (apply str (map #(char %) buf)))))

(defn create-program-from-source
  "Takes translated source (in GLSL, not s-expressions) for two shaders, and combines them into a program"
  [vertex-source fragment-source]
  (let [vertex-shader     (gl-create-shader :vertex-shader)
        fragment-shader   (gl-create-shader :fragment-shader)
        program           (gl-create-program)]
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
    (if *verbose* (println "Linked.\n"))
    (struct-map program-struct
      :vertex vertex-shader :fragment fragment-shader :program program :uniforms (ref {}))))

