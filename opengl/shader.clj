;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.shader)

(use 'penumbra.opengl.core
     'penumbra.opengl.texture
     'clojure.contrib.def)

(import '(java.nio ByteBuffer IntBuffer)
        '(com.sun.opengl.util BufferUtil))

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

(def *verbose* true)

(defmacro gl-query-status [query-fn setting fn-name]
  `(defn- ~fn-name [param#]
    (let [a# (int-array 1)]
      (~query-fn param# ~setting a# 0)
      (not (zero? (nth a# 0))))))

(gl-query-status gl-get-shader :compile-status shader-compiled?)
(gl-query-status gl-get-program :link-status program-linked?)
(gl-query-status gl-get-program :validate-status program-valid?)

(defn- get-shader-log [shader]
  (let [buf (make-array Byte/TYPE 1024)]
    (gl-get-shader-info-log shader 1024 (int-array 1) 0 buf 0)
    (.trim #^String (apply str (map #(char %) buf)))))

(defn- load-source [shader source]
  (let [a (make-array String 1)]
    (aset a 0 source)
    (gl-shader-source shader 1 a nil))
  (gl-compile-shader shader)
  (if *verbose*
    (println (str "-------------" source)))
  (if (shader-compiled? shader)
    (if *verbose* (println "Compiled."))
    (throw (Exception. (str "Error compiling shader: " (get-shader-log shader))))))

(defn- get-program-log [program]
  (let [buf (make-array Byte/TYPE 4096)]
    (gl-get-program-info-log program 4096 (int-array 1) 0 buf 0)
    (.trim (apply str (map #(char %) buf)))))

(defn create-program-from-source [vertex-source fragment-source]
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

(defn bind-program [program]
  (gl-use-program (:program program)))

;;;;;;;;;;;;;;;;;;;;

(defn third [expr] (-> expr next second))

(defn first? [val expr] (= (first expr) val))

(defn indent [exprs]
  (apply str (map #(str "  " %) exprs)))

(defn swizzle? [expr]
  (and (seq? expr) (= \. (-> expr first name first))))

(defn translate-shader-keyword [k]
  (str
    "gl_"
    (apply str
      (map
        #(str (.. % (substring 0 1) (toUpperCase)) (. % substring 1 (count %)))
        (seq (.split (name k) "-"))))))

(defn translate-assignment [expr]
  (cond
    (keyword? expr) (translate-shader-keyword expr)
    (not (seq? expr)) (str expr)
    (swizzle? expr) (str (apply str (interpose " " (map name (rest expr)))) (-> expr first name))
    :else (apply str (interpose " " (map #(.replace (name %) \- \_) expr)))))

(defmacro infix-test [symbol expr]
  `(and (= 3 (count ~expr)) (first? ~symbol ~expr)))

(defmacro infix-translation [s expr]
  `(str "(" (-> ~expr second translate-expr) " " ~s " " (-> ~expr third translate-expr) ")"))

(defmacro unary-test [symbol expr]
  `(and (= 2 (count ~expr)) (first? ~symbol ~expr)))

(defmacro unary-translation [s expr]
  `(str "(" ~s (-> ~expr second translate-expr) ")"))

(defmacro assignment-test [symbol expr]
  `(and (= 3 (count ~expr)) (first? ~symbol ~expr)))

'(defmacro assignment-translation [s expr]
  `(str (-> ~expr second translate-assignment) " " ~s " " (-> ~expr third translate-expr)))

(defmacro assignment-translation [s expr]
  `(str (-> ~expr second translate-assignment) " " ~s " " (-> ~expr third translate-expr)))
   
(defn translate-expr [expr]
  (cond
    (keyword? expr)             (translate-shader-keyword expr)
    (not (seq? expr))           (.replace (str expr) \- \_)
    (unary-test '- expr)        (unary-translation "-" expr)
    (unary-test 'not expr)      (unary-translation "!" expr)
    (assignment-test '= expr)   (assignment-translation "=" expr)
    (assignment-test '+= expr)  (assignment-translation "+=" expr)
    (assignment-test '-= expr)  (assignment-translation "+=" expr)
    (assignment-test '*= expr)  (assignment-translation "*=" expr)
    (infix-test '+ expr)        (infix-translation "+" expr)
    (infix-test '- expr)        (infix-translation "-" expr)
    (infix-test '/ expr)        (infix-translation "/" expr)
    (infix-test '* expr)        (infix-translation "*" expr)
    (infix-test '== expr)       (infix-translation "==" expr)
    (infix-test 'and expr)      (infix-translation "&&" expr)
    (infix-test 'or expr)       (infix-translation "||" expr)
    (infix-test 'xor expr)      (infix-translation "^^" expr)
    (infix-test '< expr)        (infix-translation "<" expr)
    (infix-test '<= expr)       (infix-translation "<=" expr)
    (infix-test '>= expr)       (infix-translation ">=" expr)
    (infix-test '> expr)        (infix-translation ">" expr)
    (infix-test 'nth expr)      (str (-> expr second translate-expr) "[" (-> expr third translate-expr) "]")

    ;Swizzling
    (and
      (= 2 (count expr))
      (swizzle? expr))          (str (-> expr second translate-expr) (-> expr first str))
    
    ;Function calls
    :else                       (str
                                  (first expr)
                                  "(" (apply str (interpose ", " (map translate-expr (rest expr)))) ")")))

(defn translate-exprs [exprs]
  (map #(str (translate-expr %) ";\n") exprs))

(defn translate-main-fn [expr]
  (str
    "void main()\n{\n"
    (if (seq? (first expr))
      (apply str (indent (translate-exprs expr)))
      (str "  " (translate-expr expr) ";\n"))
    "}\n"))

(defn translate-shader [decl exprs]
  (str
    "\n"
    (if (seq? (first decl))
      (apply str (map #(str (translate-assignment %) ";\n") decl))
      (str (translate-assignment decl) ";\n"))
    "\n"
    (translate-main-fn exprs)))

(defn create-program [decl vertex fragment]
  (let [vertex-source (translate-shader decl vertex)
        fragment-source (translate-shader decl fragment)]
    (create-program-from-source vertex-source fragment-source)))



