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
  (:import (java.nio ByteBuffer IntBuffer FloatBuffer))
  (:import (org.lwjgl BufferUtils)))

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

(gl-import glUniform1f uniform-1f)
(gl-import glUniform2f uniform-2f)
(gl-import glUniform3f uniform-3f)
(gl-import glUniform4f uniform-4f)

;;;;;;;;;;;;;;;;;;;;
;OpenGL shader functions

(defvar *verbose* true
  "Full feedback on shader compilation.")

(defmacro- gl-query-info
  [query-fn setting fn-name]
  `(defn- ~fn-name [param#]
    (let [a# (int-array 1)]
      (~query-fn param# ~setting (IntBuffer/wrap a#))
      (first a#))))

(defmacro- gl-query-status
  [query-fn setting fn-name]
  (let [fn-name* (symbol (str fn-name "*"))]
    `(do
       (gl-query-info ~query-fn ~setting ~fn-name*)
       (defn ~fn-name [param#]
         (not (zero? (~fn-name* param#)))))))

(gl-query-status gl-get-shader :compile-status shader-compiled?)
(gl-query-status gl-get-program :link-status program-linked?)
(gl-query-status gl-get-program :validate-status program-valid?)
(gl-query-info gl-get-shader :info-log-length shader-info-length)
(gl-query-info gl-get-program :info-log-length program-info-length)

(defn- get-shader-log [shader]
  (let [len (shader-info-length shader)
        buf (BufferUtils/createByteBuffer len)]
    (gl-get-shader-info-log shader (IntBuffer/wrap (int-array [len])) buf)
    (let [ary (byte-array len)]
      (.get buf ary)
      (.trim (String. ary)))))

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
      (throw (Exception. (str "*** Error compiling shader:\n" (get-shader-log shader)))))))

(defn- get-program-log [program]
  (let [len (program-info-length program)
        buf (BufferUtils/createByteBuffer len)]
    (gl-get-program-info-log program (IntBuffer/wrap (int-array [len])) buf)
    (let [ary (byte-array len)]
      (.get buf ary)
      (.trim (String. ary)))))

(defn compile-source
  "Takes translated source (in GLSL, not s-expressions) for two shaders, and combines them into a program.
   Usage:
   (create-program-from-source
     :vertex vertex-source
     :fragment fragment-source
     :geometry geometry-source)"
  [& sources]
  (let [src (apply hash-map sources)
        program (gl-create-program)]
    (when-let [source (:vertex src)]
      (let [shader (gl-create-shader :vertex-shader)]
        (load-source shader source)
        (gl-attach-shader program shader)))
    (when-let [source (:geometry src)]
      (let [shader (gl-create-shader :geometry-shader)]
        (load-source shader source)
        (gl-attach-shader program shader)))
    (when-let [source (:fragment src)]
      (let [shader (gl-create-shader :fragment-shader)]
        (load-source shader source)
        (gl-attach-shader program shader)))
    (gl-link-program program)
    (when-not (program-linked? program)
      (throw (Exception. (str "*** Error linking program:\n" (get-program-log program)))))
    (gl-validate-program program)
    (when-not (program-valid? program)
      (throw (Exception. (str "*** Error validating program:\n" (get-program-log program)))))
    (if *verbose* (println "Linked.\n"))
    {:program program
     :uniforms (ref {})
     :sources src}))

