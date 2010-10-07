;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.translate.c
  (:use [penumbra.translate core]
        [clojure.contrib
         (def :only (defmacro-))]))
  
;;;;;;;;;;;;;;;;;;;

(defmulti transformer
  #(if (seq? %) (id (first %)) nil)
  :default nil)

(defmulti generator
  #(if (seq? %) (id (first %)) nil)
  :default nil)

(declare special-parse-case?)

(defmulti parser
  #(if (special-parse-case? %) nil (id (first %)))
  :default :function)

(defmulti tagger
  #(if (seq? %) (id (first %)) nil)
  :default nil)

(defn translate-c [x]
  (binding [*generator* generator, *transformer* transformer, *parser* parser, *tagger* tagger]
    (translate-expr x)))

(defn transform-c [x]
  (binding [*generator* generator, *transformer* transformer, *tagger* tagger]
    (transform-expr x)))

;;;;;;;;;;;;;;;;;;;;
;;symbol metadata tagging

(defmethod tagger nil [x]
  x)

(defmethod tagger 'for [x]
  (add-meta x :scope true))

(defmethod tagger 'defn [x]
  (add-meta x :scope true))

;;;;;;;;;;;;;;;;;;;;
;;shader macros

(defmethod transformer nil [x]
  x)

(defn- wrap-scope [x body]
  (if (empty? x)
    (list 'scope body)
    (add-meta
     (list 'scope (add-meta (list '<- (first x) (second x)) :let true) (wrap-scope (nnext x) body))
     :scope true)))

(defmethod transformer 'let
  [x]
  (wrap-scope (second x) (nnext x)))

(defn- unwind-stack [term x]
  (if (seq? x)
    `(~(first x) ~term ~@(rest x))
    `(~x ~term)))

(defmethod transformer '->
  [x]
  (let [term (second x)
        x    (nnext x)]
    (reduce unwind-stack term x)))

(defmethod transformer 'dotimes
  [x]
  (let [[var limit] (second x)]
    (list* 'for `[(<- ~var (int 0)) (< ~var (int ~limit)) (++ ~var)] (nnext x))))

(defn- wrap-cond [pred clause & rest]
  (if (= pred :else)
    clause
    (if (empty? rest)
      (list 'if pred clause)
      (list 'if pred clause (apply wrap-cond rest)))))

(defmethod transformer 'cond
  [x]
  (apply wrap-cond (rest x)))

;;;;;;;;;;;;;;;;;;;;
;shader generators

(defmethod generator nil [x]
  (if (seq? x)
    (apply concat (map try-generate x))
    nil))

(defmethod generator 'import [x]
  (apply concat
    (map
      (fn [lst]
        (let [namespace (first lst)]
          (map #(var-get (intern namespace (symbol %))) (next lst))))
      (next x))))

(defmethod parser 'import [x] "")

;;;;;;;;;;;;;;;;;;;;
;shader parser

(defn swizzle?
  "Any function starting with a '.' is treated as a member access.
  (.xyz position) -> position.xyz"
  [x]
  (and (seq? x) (-> x first symbol?) (= \. (-> x first name first))))

(defn- third [x] (-> x next second))
(defn- fourth [x] (-> x nnext second))
(defn- first= [x k] (and (seq? x) (= k (keyword (first x)))))

(defn- parse-assignment-left
  "Parses the l-value in an assignment xession."
  [x]
  (cond
    (swizzle? x)
      (str (apply parse (interpose " " (map name (next x)))) (-> x first name))
    (symbol? x)
      (let [x* (symbol (.replace (name x) \- \_))]
        (if (:first-appearance (meta x))
          (let [type (typeof x)
                modifiers (:modifiers (meta x))]
            (->> (list* x* type modifiers) (map name) reverse (interpose " ") (apply str) .trim))
          (str x*)))
    (first= x :nth)
      (str (parse-assignment-left (second x)) "[" (parse (third x)) "]")
    (and (seq? x) (empty? x))
      ""
    :else
      (parse x)))

(defn- parse-assignment-right
  "Parses the r-value in an assignment xessions."
  [x]
  (cond
   :else (parse x)))

(defn- special-parse-case? [x]
  (or
   (swizzle? x)
   (not (seq? x))
   (-> x first seq?)))

(def *line-terminator* ";")

(defmethod parser nil
  ;handle base cases
  [x]
  (cond
   (swizzle? x)      (str (-> x second parse) (-> x first str))
   (number? x)       (str x)
   (not (seq? x))    (.replace (str x) \- \_)
   (-> x first seq?) (parse-lines x *line-terminator*)
   :else                ""))

(defmethod parser :function
  ;transforms (a b c d) into a(b, c, d)
  [x]
  (str
    (.replace (name (first x)) \- \_)
    "(" (apply str (interpose ", " (map parse (next x)))) ")"))

(defn- concat-operators
  "Interposes operators between two or more operands, enforcing left-to-right evaluation.
  (- a b c) -> ((a - b) - c)"
  [op x]
  (if (> 2 (count x)) (throw (Exception. "Must be at least two operands.")))
  (if (= 2 (count x))
    (str "(" (parse (second x)) " " op " " (parse (first x)) ")")
    (str "(" (concat-operators op (rest x)) " " op " " (parse (first x)) ")")))

(defmacro- def-infix-parser
  "Defines an infix operator
  (+ a b) -> a + b"
  [op-symbol op-string]
  `(defmethod parser ~op-symbol [x#]
    (concat-operators ~op-string (reverse (next x#)))))

(defmacro- def-unary-parser
  "Defines a unary operator
  (not a) -> !a"
  [op-symbol op-string]
  `(defmethod parser ~op-symbol [x#]
     (str ~op-string (parse (second x#)))))

(def *assignment* false)

(defmacro- def-assignment-parser
  "Defines an assignment operator, making use of parse-assignment for the l-value
  (set! a b) -> a = b"
  [op-symbol op-string]
  `(do
    (defmethod parser ~op-symbol [a#]
      (if (= 2 (count a#))
        (str (parse-assignment-left (second a#)))
        (str (parse-assignment-left (second a#)) " " ~op-string " " (binding [*assignment* true]
                                                                      (parse-assignment-right (third a#))))))
    (defmethod tagger ~op-symbol [b#]
      (let [s1# (first b#)
            s2# (second b#)
            s3# (third b#)]
        (cond
         (not (meta? s2#))
         b#
         (= 2 (count b#))
         (list
          s1#
          (add-meta s2# :assignment (symbol? s2#), :defines s2#))
         :else
         (do
           (list
           s1#
           (add-meta s2# :assignment (symbol? s2#), :defines s2#, :numeric-value (when (number? s3#) s3#))
           (add-meta s3# :defines s2#))))))))

(defmacro- def-scope-parser
  "Defines a wrapper for any keyword that wraps a scope
  (if a b) -> if (a) { b }"
  [scope-symbol scope-fn]
  `(defmethod parser ~scope-symbol [x#]
    (let [[header# body#] (~scope-fn x#)]
      (str header# "\n{\n" (indent (parse body#)) "}\n"))))

(def-infix-parser '+ "+")
(def-infix-parser 'div "/")
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
(def-assignment-parser '<- "=")
(def-assignment-parser '+= "+=")
(def-assignment-parser '-= "-=")
(def-assignment-parser '*= "*=")

(defmethod parser '-
  ;the - symbol can either be a infix or unary operator
  [x]
  (if (>= 2 (count x))
    (str "-" (parse (second x)))
    (concat-operators "-" (reverse (next x)))))

(defmethod parser 'nth
  [x]
  (str (parse (second x)) "[" (parse (third x)) "]"))

(defmethod parser 'do
  [x]
  (parse (next x)))

(defmethod parser 'when
  [x]
  (str
   "if (" (parse (second x)) ")"
   "\n{\n" (indent (parse-lines (nnext x) ";")) "}\n"))

(defmethod parser 'if
  [x]
  (if *assignment*
    (str "(" (parse (second x))
         " ? " (parse (third x))
         " : " (parse (or (fourth x) (third x))) ")")
    (str "if (" (parse (second x)) ")"
         "\n{\n" (indent (parse-lines (third x) ";")) "}\n"
         (if (< 3 (count x))
           (str "else\n{\n" (indent (parse-lines (fourth x) ";")) "}\n")
           ""))))

(defmethod parser 'return
  [x]
  (str "return " (parse (second x))))

(defmethod parser 'break
  [x]
  (str "break;\n"))

(defmethod parser 'continue
  [x]
  (str "continue;\n"))

(defmethod parser 'scope
  [x]
  (str "{\n" (indent (parse-lines (next x) ";")) "}\n"))

(defmethod parser 'for
  [x]
  (let [[a b c] (second x)]
    (str
     "for ("
     (binding [*line-terminator* ", "]
       (.replace
        (str (parse a) "; "
             (parse b) "; "
             (parse c) ")")
        "\n" ""))
     "\n{\n"
     (indent (parse (nnext x)))
     "}")))

(def-scope-parser
  'defn
  (fn [x]
    [(str
      (second x) " " (.replace (name (third x)) \- \_)
      "(" (apply str (interpose ", " (map parse-assignment-left (fourth x)))) ")")
     (drop 4 x)]))

(defmethod tagger 'defn [x]
  (list*
    (first x) (second x) (third x)
    (vec (map #(add-meta % :assignment true, :defines %) (fourth x)))
    (drop 4 x)))
