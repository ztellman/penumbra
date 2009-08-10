;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.opengl.translate
  (:use [clojure.contrib (def :only (defmacro-)) pprint])
  (:use [penumbra.opengl.core])
  (:require [clojure.zip :as zip])
  (:import (java.text ParseException))
  (:import (java.io StringWriter)))

;;;;;;;;;;;;;;;;;;;

(defn realize [s]
  (if (seq? s) (doall s) s))

(defn- str-pprint [expr]
  (let [s (new StringWriter)]
    (pprint expr s)
    (str s)))

;;;;;;;;;;;;;;;;;;;

(defmulti shader-macro
  #(if (seq? %) (first %) nil)
  :default :none)

(defmulti shader-generator
  #(if (seq? %) (first %) nil)
  :default :none)

(declare special-parse-case?)

(defmulti shader-parser
  #(if (special-parse-case? %) nil (first %))
  :default :function)

(defn parse [expr]
  (try
    (realize (shader-parser expr))
    (catch ParseException e
      (throw e))
    (catch Exception e
      (throw (ParseException. (str "\nError while parsing:\n" (str-pprint expr) (.getMessage e)) 0)))))

;;;;;;;;;;;;;;;;;;;;

(defn- try-apply [fun expr]
  (try
    (realize (fun expr))
    (catch Exception e
      (throw (ParseException. (str "\nError while transforming:\n" (str-pprint expr) (.getMessage e)) 0)))))

(defn tree-map [tree fun]
  (if (empty? tree)
    ()
    (loop [z (zip/seq-zip tree)]
      (if (zip/end? z)
        (zip/root z)
        (recur (zip/next (zip/replace z (try-apply fun (zip/node z)))))))))

(defn- transform-exprs [expr]
  (tree-map expr shader-macro))  

(defn- try-generate [expr]
  (try
    (realize (shader-generator expr))
    (catch Exception e
      (throw (ParseException. (str "\nError while generating:\n" (str-pprint expr) (.getMessage e)) 0)))))

(defn- generate-exprs [expr]
  (loop [body (list expr)
         tail (transform-exprs (try-generate expr))]
    (if (empty? tail)
     body
     (recur
       (concat body (if (seq? (ffirst tail)) (apply concat tail) tail))
       (transform-exprs (try-generate tail))))))

(defn- parse-lines
  "Maps shader-parser over a list of s-expressions, filters out empty lines,
  and optionally adds a terminating character."
  ([exprs] (parse-lines "" exprs))
  ([termination exprs]
     (if (and
          (seq? exprs)
          (= 1 (count exprs))
          (seq? (first exprs)))
      (parse-lines termination (first exprs))
      (let [exprs (if (seq? (first exprs)) exprs (list exprs))
            parsed-exprs (map #(parse %) exprs)
            filtered-exprs (filter #(not= (.trim %) "") parsed-exprs)
            terminated-exprs (map #(if (.endsWith % "\n") % (str % termination "\n")) filtered-exprs)]
        (apply str terminated-exprs)))))

(defn indent
  "Indents every line two spaces."
  [s]
  (let [lines (seq (.split s "\n"))]
    (str (apply str (interpose "\n" (map #(str "  " %) lines))) "\n")))

(defn translate
  "Core function for translate-shader.  Useful for testing phrases."
  [expr]
  (parse-lines (reverse (generate-exprs (transform-exprs expr)))))

(defn translate-shader
  ([exprs] (translate-shader '() exprs))
  ([decl exprs]
     (let [parsed-decl
           (if (empty? decl)
             ""
             (parse-lines ";" (map #(list 'declare %) decl)))]
       (str parsed-decl (translate (list 'main exprs))))))

;;;;;;;;;;;;;;;;;;;;
;;shader macros

(defn- transform-vec
  "implicitly transforms [a b] into (vec* a b)"
  [v]
  (let [num (count v)
        cls (class (first v))]
    (cond
     (and (<= num 4) (= Integer cls))  `(~(symbol (str "ivec" num)) ~@v)
     (and (= Boolean cls))             `(~(symbol (str "bvec" num)) ~@v)
     :else                             `(~(symbol (str "vec" (count v))) ~@(map #(float %) v)))))

(defmethod shader-macro :none [expr]
  (cond
    (and
     (vector? expr)
     (not (or
           (seq? (first expr))
           (symbol? (first expr))))) (transform-vec expr)
    :else expr))

(defmethod shader-macro 'let
  [expr]
  (concat
    '(do)
    (map #(list 'set! (first %) (second %)) (partition 2 (second expr)))
    (nnext expr)))

(defn- unwind-stack [term expr]
  (if (seq? expr)
    `(~(first expr) ~term ~@(rest expr))
    `(~expr ~term)))

(defmethod shader-macro '->
  [expr]
  (let [term  (second expr)
        expr  (nnext expr)]
    (reduce unwind-stack term expr)))

;;;;;;;;;;;;;;;;;;;;
;shader generators

(defmethod shader-generator :none [expr]
  (if (seq? expr)
    (apply concat (map try-generate expr))
    nil))

(defmethod shader-generator 'import [expr]
  (apply concat
    (map
      (fn [lst]
        (let [namespace (first lst)]
          (map #(var-get (intern namespace %)) (next lst))))
      (next expr))))

(defmethod shader-parser 'import [expr] "")

;;;;;;;;;;;;;;;;;;;;
;shader parser

(defn- swizzle?
  "Any function starting with a '.' is treated as a member access.
  (.xyz position) -> position.xyz"
  [expr]
  (and (seq? expr) (= \. (-> expr first name first))))

(defn- third [expr] (-> expr next second))
(defn- fourth [expr] (-> expr next next second))
(defn- first= [expr sym] (and (seq? expr) (= sym (first expr))))

(defn- parse-keyword
  "Turns :model-view-matrix into gl_ModelViewMatrix."
  [k]
  (str
    "gl_"
    (apply str
      (map
        #(str (.. % (substring 0 1) toUpperCase) (. % substring 1 (count %)))
        (seq (.split (name k) "-"))))))

(defn- parse-assignment-left
  "Parses the l-value in an assignment expression."
  [expr]
  (cond
    (keyword? expr)         (parse-keyword expr)
    (swizzle? expr)         (str (apply str (interpose " " (map name (next expr)))) (-> expr first name))
    (symbol? expr)          (.replace (name expr) \- \_)
    (first= expr 'nth)      (str (parse-assignment-left (second expr)) "[" (parse (third expr)) "]")
    (empty? expr)           ""
    :else                   (apply str (interpose " " (map parse-assignment-left expr)))))

(defn- parse-assignment-right
  "Parses the r-value in an assignment expressions."
  [expr]
  (cond
    (first= expr 'if) (str "(" (parse (second expr))
                           " ? " (parse (third expr))
                           " : " (parse (fourth expr)) ")")
    :else             (parse expr)))

(defn- special-parse-case? [expr]
  (or
    (swizzle? expr)
    (not (seq? expr))))

(defmethod shader-parser nil
  ;handle base cases
  [expr]
  (cond
    (keyword? expr)    (parse-keyword expr)
    (swizzle? expr)    (str (-> expr second parse) (-> expr first str))
    (not (seq? expr))  (.replace (str expr) \- \_)
    :else              ""))

(defmethod shader-parser :function
  ;transforms (a b c d) into a(b, c, d)
  [expr]
  (str
    (.replace (name (first expr)) \- \_)
    "(" (apply str (interpose ", " (map parse (next expr)))) ")"))

(defn- concat-operators
  "Interposes operators between two or more operands, enforcing left-to-right evaluation.
  (- a b c) -> ((a - b) - c)"
  [op expr]
  (if (> 2 (count expr)) (throw (Exception. "Must be at least two operands.")))
  (if (= 2 (count expr))
    (str "(" (parse (second expr)) " " op " " (parse (first expr)) ")")
    (str "(" (concat-operators op (rest expr)) " " op " " (parse (first expr)) ")")))

(defmacro- def-infix-parser
  "Defines an infix operator
  (+ a b) -> a + b"
  [op-symbol op-string]
  `(defmethod shader-parser ~op-symbol [expr#]
    (concat-operators ~op-string (reverse (next expr#)))))

(defmacro- def-unary-parser
  "Defines a unary operator
  (not a) -> !a"
  [op-symbol op-string]
  `(defmethod shader-parser ~op-symbol [expr#]
    (str ~op-string (parse (second expr#)))))

(defmacro- def-assignment-parser
  "Defines an assignment operator, making use of parse-assignment for the l-value
  (set! a b) -> a = b"
  [op-symbol op-string]
  `(defmethod shader-parser ~op-symbol [expr#]
    (if (= 2 (count expr#))
      (str (parse-assignment-left (second expr#)))
      (str (parse-assignment-left (second expr#)) " " ~op-string " " (parse-assignment-right (third expr#))))))

(defmacro- def-scope-parser
  "Defines a wrapper for any keyword that wraps a scope
  (if a b) -> if (a) { b }"
  [scope-symbol scope-fn]
  `(defmethod shader-parser ~scope-symbol [expr#]
    (let [[header# body#] (~scope-fn expr#)]
      (str header# "\n{\n" (indent (parse-lines ";" body#)) "}\n"))))

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
(def-unary-parser 'inc "++")
(def-unary-parser 'dec "--")
(def-assignment-parser 'declare "")
(def-assignment-parser 'set! "=")
(def-assignment-parser '+= "+=")
(def-assignment-parser '-= "-=")
(def-assignment-parser '*= "*=")

(defmethod shader-parser '-
  ;the - symbol can either be a infix or unary operator
  [expr]
  (if (>= 2 (count expr))
    (str "-" (parse (second expr)))
    (concat-operators "-" (reverse (next expr)))))

(defmethod shader-parser 'nth
  [expr]
  (str (parse (second expr)) "[" (third expr) "]"))

(def-scope-parser 'main
  (fn [expr] ["void main()" (next expr)]))

(defmethod shader-parser 'do
  [expr]
  (parse-lines ";" (next expr)))

(defmethod shader-parser 'if
  [expr]
  (str
    "if (" (parse (second expr)) ")"
    "\n{\n" (indent (parse-lines ";" (third expr))) "}\n"
    (if (< 3 (count expr))
      (str "else\n{\n" (indent (parse-lines ";" (fourth expr))) "}\n")
      "")))

(defmethod shader-parser 'return
  [expr]
  (str "return " (parse (second expr))))

(def-scope-parser
  'defn
  (fn [expr]
    [(str
      (second expr) " " (.replace (name (third expr)) \- \_)
      "(" (apply str (interpose ", " (map parse-assignment-left (fourth expr)))) ")")
     (drop 4 expr)]))