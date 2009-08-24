;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.translate.c
  (:use [penumbra.translate.util])
  (:use [clojure.contrib (def :only (defmacro-))]))
  
;;;;;;;;;;;;;;;;;;;

(defmulti c-transformer
  #(if (seq? %) (first %) nil)
  :default :none)

(defmulti c-generator
  #(if (seq? %) (first %) nil)
  :default :none)

(declare special-parse-case?)

(defmulti c-parser
  #(if (special-parse-case? %) nil (first %))
  :default :function)

(defn translate-c [expr]
  (binding [*generator* c-generator, *transformer* c-transformer, *parser* #(try-parse % c-parser)]
    (translate-expr expr)))

(defn- parse [expr]
  (*parser* expr))

;;;;;;;;;;;;;;;;;;;;
;;shader macros

(defmethod c-transformer :none [expr]
  expr)

(defmethod c-transformer 'let
  [expr]
  (concat
    '(do)
    (map #(list 'set! (first %) (second %)) (partition 2 (second expr)))
    (nnext expr)))

(defn- unwind-stack [term expr]
  (if (seq? expr)
    `(~(first expr) ~term ~@(rest expr))
    `(~expr ~term)))

(defmethod c-transformer '->
  [expr]
  (let [term  (second expr)
        expr  (nnext expr)]
    (reduce unwind-stack term expr)))

;;;;;;;;;;;;;;;;;;;;
;shader generators

(defmethod c-generator :none [expr]
  (if (seq? expr)
    (apply concat (map try-generate expr))
    nil))

(defmethod c-generator 'import [expr]
  (apply concat
    (map
      (fn [lst]
        (let [namespace (first lst)]
          (map #(var-get (intern namespace %)) (next lst))))
      (next expr))))

(defmethod c-parser 'import [expr] "")

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

(defn- parse-assignment-left
  "Parses the l-value in an assignment expression."
  [expr]
  (cond
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

(defmethod c-parser nil
  ;handle base cases
  [expr]
  (cond
    (swizzle? expr)    (str (-> expr second parse) (-> expr first str))
    (not (seq? expr))  (.replace (str expr) \- \_)
    :else              ""))

(defmethod c-parser :function
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
  `(defmethod c-parser ~op-symbol [expr#]
    (concat-operators ~op-string (reverse (next expr#)))))

(defmacro- def-unary-parser
  "Defines a unary operator
  (not a) -> !a"
  [op-symbol op-string]
  `(defmethod c-parser ~op-symbol [expr#]
    (str ~op-string (parse (second expr#)))))

(defmacro- def-assignment-parser
  "Defines an assignment operator, making use of parse-assignment for the l-value
  (set! a b) -> a = b"
  [op-symbol op-string]
  `(defmethod c-parser ~op-symbol [expr#]
    (if (= 2 (count expr#))
      (str (parse-assignment-left (second expr#)))
      (str (parse-assignment-left (second expr#)) " " ~op-string " " (parse-assignment-right (third expr#))))))

(defmacro- def-scope-parser
  "Defines a wrapper for any keyword that wraps a scope
  (if a b) -> if (a) { b }"
  [scope-symbol scope-fn]
  `(defmethod c-parser ~scope-symbol [expr#]
    (let [[header# body#] (~scope-fn expr#)]
      (str header# "\n{\n" (indent (parse-lines body# ";")) "}\n"))))

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

(defmethod c-parser '-
  ;the - symbol can either be a infix or unary operator
  [expr]
  (if (>= 2 (count expr))
    (str "-" (parse (second expr)))
    (concat-operators "-" (reverse (next expr)))))

(defmethod c-parser 'nth
  [expr]
  (str (parse (second expr)) "[" (third expr) "]"))

(defmethod c-parser 'do
  [expr]
  (parse-lines (next expr) ";"))

(defmethod c-parser 'if
  [expr]
  (str
    "if (" (parse (second expr)) ")"
    "\n{\n" (indent (parse-lines (third expr) ";")) "}\n"
    (if (< 3 (count expr))
      (str "else\n{\n" (indent (parse-lines (fourth expr) ";")) "}\n")
      "")))

(defmethod c-parser 'return
  [expr]
  (str "return " (parse (second expr))))

(def-scope-parser
  'defn
  (fn [expr]
    [(str
      (second expr) " " (.replace (name (third expr)) \- \_)
      "(" (apply str (interpose ", " (map parse-assignment-left (fourth expr)))) ")")
     (drop 4 expr)]))