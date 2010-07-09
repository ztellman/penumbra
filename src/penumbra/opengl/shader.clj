;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.shader
  (:use [clojure.contrib.def :only (defmacro- defvar)])
  (:use [clojure.contrib.seq :only (indexed)])
  (:use [penumbra.opengl.core])
  (:use [penumbra.glsl.core])
  (:use [penumbra.translate.core])
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

(gl-import- glUniform1i uniform-1i)
(gl-import- glUniform2i uniform-2i)
(gl-import- glUniform3i uniform-3i)
(gl-import- glUniform4i uniform-4i)

(gl-import- glUniform1f uniform-1f)
(gl-import- glUniform2f uniform-2f)
(gl-import- glUniform3f uniform-3f)
(gl-import- glUniform4f uniform-4f)

;;;

(gl-import- glUseProgram gl-use-program)
(gl-import- glGetUniformLocation gl-get-uniform-location)

(defn bind-program
  "Calls glUseProgram."
  [program]
  (gl-use-program (if (nil? program) 0 (:program program))))

(defn with-program
  "Binds program within inner-scope."
  [program body]
  (let [prev-program *program*]
    (try
     (binding [*program* program, *uniforms* (:uniforms program), *attributes* (:attributes program)]
       (bind-program program)
       (body))
     (finally
      (if (not= prev-program program)
        (bind-program prev-program))))))

(defn- int? [p]
  (let [cls (class p)]
    (if (or (= cls Integer) (= cls Integer/TYPE)) true false)))

(defn uniform-location [variable]
  (if-let [location (@*uniforms* variable)]
    location
    (let [ary (.getBytes (str (.replace (name variable) \- \_) "\0"))
          uniform-buf (-> (BufferUtils/createByteBuffer (count ary))
                          (.put ary)
                          .rewind)
          loc (gl-get-uniform-location (:program *program*) uniform-buf)]
      (dosync (alter *uniforms* #(assoc % variable loc)))
      loc)))

(defn uniform [variable & args]
  (let [loc     (uniform-location variable)
        is-int  (int? (first args))
        args    (vec args) ;;(vec (map (if is-int int float) args))
        ]
    (if is-int
      (condp = (count args)
        1 (uniform-1i loc (args 0))
        2 (uniform-2i loc (args 0) (args 1))
        3 (uniform-3i loc (args 0) (args 1) (args 2))
        4 (uniform-4i loc (args 0) (args 1) (args 2) (args 3)))
      (condp = (count args)
        1 (uniform-1f loc (args 0))
        2 (uniform-2f loc (args 0) (args 1))
        3 (uniform-3f loc (args 0) (args 1) (args 2))
        4 (uniform-4f loc (args 0) (args 1) (args 2) (args 3))))))

;;;shader

(defvar *verbose* true
  "Full feedback on shader compilation.")

(defmacro- gl-query-info
  [query-fn setting fn-name]
  `(defn- ~fn-name [param#]
    (let [buf# (BufferUtils/createIntBuffer 1)]
      (~query-fn param# ~setting buf#)
      (.get buf# 0))))

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
    (gl-get-shader-info-log shader (-> (BufferUtils/createIntBuffer len) (.put (int-array [len])) .rewind) buf)
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
        ary (.getBytes source)
        source-buf (-> (BufferUtils/createByteBuffer (count ary)) (.put ary) .rewind)]
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
    (gl-get-program-info-log program (-> (BufferUtils/createIntBuffer len) (.put (int-array [len])) .rewind) buf)
    (let [ary (byte-array len)]
      (.get buf ary)
      (.trim (String. ary)))))

(defn compile-source
  "Takes translated source (in GLSL, not s-expressions) for two shaders, and combines them into a program.
   Usage:
   (compile-source
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
      (let [shader (gl-create-shader :geometry-shader-ext)]
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
    (when *verbose*
      (println "Linked.\n"))
    {:program program
     :uniforms (ref {})
     :attributes (ref {})
     :sources src}))

