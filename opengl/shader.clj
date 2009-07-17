;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.shader)

(use 'penumbra.opengl.core
     'penumbra.opengl.texture)

(import '(java.nio ByteBuffer IntBuffer)
        '(com.sun.opengl.util BufferUtil))

;;;;;;;;;;;;;;;;;;

(gl-import glCreateShader gl-create-shader)
(gl-import glShaderSource gl-shader-source)
(gl-import glCompileShader gl-compile-shader)
(gl-import glCreateProgram gl-create-program)
(gl-import glAttachShader gl-attach-shader)
(gl-import glLinkProgram gl-link-program)
(gl-import glUseProgram gl-use-program)
(gl-import glGetShaderInfoLog gl-get-shader-info-log)
(gl-import glGetShaderiv gl-get-shader)
(gl-import glGetProgramInfoLog gl-get-program-info-log)
(gl-import glGetProgramiv gl-get-program)

(defstruct gpu-program :vertex :fragment :program)

;;;;;;;;;;;;;;;;;;;;

(defn get-shader-log [shader]
  (let [buf (make-array Byte/TYPE 1024)]
    (gl-get-shader-info-log shader 1024 (int-array 1) 0 buf 0)
    (.trim (apply str (map #(char %) buf)))))

(defn shader-compiled? [shader]
  (let [a (int-array 1)]
    (gl-get-shader shader :compile-status a 0)
    (not (zero? (nth a 0)))))

(defn load-source [shader source]
  (let [a (make-array String 1)]
    (aset a 0 source)
    (gl-shader-source shader 1 a nil))
  (gl-compile-shader shader)
  (println (get-shader-log shader))
  (if (not (shader-compiled? shader))
    (throw (Exception. (str "Shader error: " (get-shader-log shader))))))

(defn get-program-log [program]
  (let [buf (make-array Byte/TYPE 1024)]
    (gl-get-program-info-log program 1024 (int-array 1) 0 buf 0)
    (.trim (apply str (map #(char %) buf)))))

(defn get-program-log [program]
  (let [buf (ByteBuffer/allocate 1024)]
    (gl-get-program-info-log program 1024 (IntBuffer/allocate 1) buf)
    (.. buf (asCharBuffer) (toString))))

(defn program-linked? [program]
  (let [a (int-array 1)]
    (gl-get-program program :link-status a 0)
    (not (zero? (nth a 0)))))

(defn create-program [vertex-source fragment-source]
  (let [vertex-shader (gl-create-shader :vertex-shader)
        fragment-shader (gl-create-shader :fragment-shader)
        program (gl-create-program)]
    (load-source vertex-shader vertex-source)
    (load-source fragment-shader fragment-source)
    (gl-attach-shader program vertex-shader)
    (gl-attach-shader program fragment-shader)
    (gl-link-program program)
    (println (get-program-log program))
    (if (not (program-linked? program))
      (throw (Exception. (get-program-log program))))
    (struct-map gpu-program :vertex vertex-shader :fragment fragment-shader :program program)))

(defn bind-program [program]
  (gl-use-program (:program program)))

;;;;;;;;;;;;;;;;;;;;


