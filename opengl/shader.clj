;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.shader
  (:use [clojure.contrib.def :only (defmacro-)])
  (:use [penumbra.opengl.core])
  (:require [clojure.zip :as zip])
  (:import (java.nio ByteBuffer IntBuffer))
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
    (if *verbose* (println "Compiled."))
    (throw (Exception. (str "Error compiling shader: " (get-shader-log shader))))))

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
      (throw (Exception. (str "Error linking program: " (get-program-log program)))))
    (gl-validate-program program)
    (if (not (program-valid? program))
      (throw (Exception. (str "Error validating program: "(get-program-log program)))))
    (if *verbose* (println "Linked."))
    (struct-map gpu-program :vertex vertex-shader :fragment fragment-shader :program program)))

(defn bind-program
  [program]
  (gl-use-program (if (nil? program) 0 (:program program))))

;;;;;;;;;;;;;;;;;;;;
;shader macros

(defmulti transform-shader
  #(if (seq? %) (first %) nil)
  :default :none)

(defmethod transform-shader :none [expr]
  expr)

(defmethod transform-shader 'let
  [expr]
  (map #(list 'set (first %) (second %)) (partition 2 (second expr))))

(defn transform
  "Maps transform-shader over the entire source tree."
  [expr]
  (loop [z (zip/seq-zip expr)]
    (if (zip/end? z)
      (zip/root z)
      (recur (zip/next (zip/replace z (transform-shader (zip/node z))))))))

;;;;;;;;;;;;;;;;;;;;
;shader generators

(defmulti generate-shader
  #(if (seq? %) (first %) nil)
  :default :none)

(defmethod generate-shader :none [expr]
  (if (seq? expr)
    (apply concat (map generate-shader expr))
    ()))

(defn generate [expr]
  (if (empty? (generate-shader expr))
    expr
    (loop [body expr
           tail (transform (generate-shader expr))]
      (if (empty? tail)
        body
        (recur
          (if (= body expr) (list body tail) (concat body (list tail)))
          (transform (generate-shader tail)))))))

;;;;;;;;;;;;;;;;;;;;
;shader parser

(defn- swizzle?
  "Any function starting with a '.' is treated as a member access.
  (.xyz position) -> position.xyz"
  [expr]
  (and (seq? expr) (= \. (-> expr first name first))))

(defn- third [expr] (-> expr next second))

(defn parse-keyword
  "Turns :model-view-matrix into gl_ModelViewMatrix."
  [k]
  (str
    "gl_"
    (apply str
      (map
        #(str (.. % (substring 0 1) (toUpperCase)) (. % substring 1 (count %)))
        (seq (.split (name k) "-"))))))

(defn semi-flatten
  "Within the base list, ensures no first element is also a list."
  [body]
  (reduce
    (fn [lines expr]
      (if (and (seq? expr) (seq? (first expr)))
        (concat lines (semi-flatten expr))
        (concat lines (list expr))))
    '() body))

(defn parse-assignment
  "Parses the l-value in an assignment expression."
  [expr]
  (cond
    (keyword? expr)   (parse-keyword expr)
    (swizzle? expr)   (str (apply str (interpose " " (map name (next expr)))) (-> expr first name))
    (symbol? expr)    (.replace (name expr) \- \_)
    (empty? expr)     ""
    :else             (apply str (interpose " " (map parse-assignment expr)))))

(defn- special-parse-case? [expr]
  (or
    (swizzle? expr)
    (not (seq? expr))))

(defmulti parse-shader
  #(if (special-parse-case? %) nil (first %))
  :default :function)

(defmethod parse-shader nil
  ;handle base cases
  [expr]
  (cond
    (keyword? expr)   (parse-keyword expr)
    (swizzle? expr)   (str (-> expr second parse-shader) (-> expr first str))
    (not (seq? expr)) (.replace (str expr) \- \_)
    :else             ""))

(defmethod parse-shader :function
  ;transforms (a b c d) into a(b, c, d)
  [expr]
  (str (first expr) "(" (apply str (interpose ", " (map parse-shader (next expr)))) ")"))

(defn concat-operators
  "Interposes operators between two or more operands, enforcing left-to-right evaluation.
  (- a b c) -> ((a - b) - c)"
  [op expr]
  (if (= 2 (count expr))
    (str "(" (parse-shader (second expr)) " " op " " (parse-shader (first expr)) ")")
    (str "(" (concat-operators op (rest expr)) " " op " " (parse-shader (first expr)) ")")))

(defmacro def-infix-parser
  "Defines an infix operator
  (+ a b) -> a + b"
  [op-symbol op-string]
  `(defmethod parse-shader ~op-symbol [expr#]
    (concat-operators ~op-string (reverse (next expr#)))))

(defmacro def-unary-parser
  "Defines a unary operator
  (not a) -> !a"
  [op-symbol op-string]
  `(defmethod parse-shader ~op-symbol [expr#]
    (str ~op-string (parse-shader (second expr#)))))

(defmacro def-assignment-parser
  "Defines an assignment operator, making use of parse-assignment for the l-value
  (set a b) -> a = b"
  [op-symbol op-string]
  `(defmethod parse-shader ~op-symbol [expr#]
    (if (= 2 (count expr#))
      (str (parse-assignment (second expr#)))
      (str (parse-assignment (second expr#)) " " ~op-string " " (parse-shader (third expr#))))))

(def-infix-parser '+ "+")
(def-infix-parser '/ "/")
(def-infix-parser '* "*")
(def-infix-parser '= "==")
(def-infix-parser 'and "&&")
(def-infix-parser 'or "||")
(def-infix-parser 'xor "^^")
(def-infix-parser '< "<")
(def-infix-parser '<= "<=")
(def-infix-parser '> ">")
(def-infix-parser '>= ">=")
(def-unary-parser 'not "!")
(def-assignment-parser 'declare "")
(def-assignment-parser 'set "=")
(def-assignment-parser '+= "+=")
(def-assignment-parser '-= "-=")
(def-assignment-parser '*= "*=")

(defmethod parse-shader '-
  ;the '-' symbol can either be a infix or unary operator
  [expr]
  (if (>= 2 (count expr))
    (str "-" (parse-shader (second expr)))
    (concat-operators "-" (reverse (next expr)))))

(defn parse-lines
  ([exprs] (parse-lines "" exprs))
  ([terminate-string exprs]
    (str (apply str (map #(str (parse-shader %) terminate-string "\n") exprs)))))

(defn indent [s]
  (let [lines (seq (.split s "\n"))]
    (str (apply str (interpose "\n" (map #(str "  " %) lines))) "\n")))

(defmethod parse-shader 'main [expr]
  (str
    "void main()\n{\n"
    (indent (parse-lines ";" (semi-flatten (next expr))))
    "}\n"))

(defn translate-shader
  [decl exprs]
  (str
    (parse-lines ";" (semi-flatten (map #(list 'declare %) decl))) "\n"
    (-> (list (list 'main exprs)) transform semi-flatten parse-lines)))

(defn create-program [decl vertex fragment]
  (let [vertex-source (translate-shader decl vertex)
        fragment-source (translate-shader decl fragment)]
    (create-program-from-source vertex-source fragment-source)))



