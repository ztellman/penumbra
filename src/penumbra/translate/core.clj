;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.translate.core
  (:use [clojure.contrib.pprint])
  (:use [clojure.contrib.def :only (defmacro- defvar)])
  (:import (java.text ParseException))
  (:import (java.io StringWriter))
  (:require [clojure.zip :as zip]))

;;;

(defvar *generator* nil
  "Anything returned by this is prepended to the beginning of the expression.
  Currently only used for imports, could also be used for anonymous functions.")
(defvar *parser* nil
  "Returns a string in the native language for the given s-expression.")
(defvar *transformer* nil
  "Macros, applied from leaf to root across entire expression.")
(defvar *inspector* nil
  "Returns the type of the expression.  Applied as :tag metadata.")
(defvar *assignment* nil
  "Specialized macro.  Should set :assignment metadata to true where a variable is being assigned, and pull
  type from r-value to l-value.")

;;;

(defn seq-wrap [s]
  (if (sequential? s) s (list s)))

(defn realize [s]
  (if (seq? s) (doall s) s))

(defn- str-pprint [expr]
  (let [s (new StringWriter)]
    (pprint expr s)
    (str s)))

(defn indent
  "Indents every line two spaces."
  [s]
  (let [lines (seq (.split s "\n"))]
    (str (apply str (interpose "\n" (map #(str "  " %) lines))) "\n")))

(defmacro- defn-try [name fun message]
  `(defn ~name [expr#]
    (try
      (~fun expr#)
      (catch ParseException pe#
        (throw pe#))
      (catch Exception e#
        (throw (ParseException. (str "\n" ~message "\n" (str-pprint expr#) (.getMessage e#)) 0))))))

(defmacro- defn-try-
  [name fun message]
  (list `defn-try (with-meta name (assoc (meta name) :private true)) fun message))

;;;

(defn-try- try-transform
  #(realize (*transformer* %))
  "Error while transforming")

(defn- mimic-expr [a b]
  (with-meta
    (cond
      (vector? a) (vec b)
      (map? a) (hash-map b)
      :else b)
    (merge ^a ^b)))

(defn- tree-map* ;post-order recursion, so that we can build transforms upwards 
  [x fun]
  (cond
    (not (sequential? x)) (fun x)
    (empty? x) ()
    :else (mimic-expr x (fun (map #(tree-map* % fun) x)))))

(defn tree-map [x fun]
  (loop [x (tree-map* x fun)]
    (let [x* (tree-map* x fun)]
      (if (= x x*) x (recur x*)))))

(defn- transform-exprs [expr]
  (tree-map expr try-transform))

;;;

(defn-try try-generate
  #(realize (*generator* %))
  "Error while generating")

(defn generate-exprs [expr]
  (loop [body (list expr)
         tail (-> expr try-generate transform-exprs)]
    (if (empty? tail)
     body
     (recur
       (concat body (if (seq? (ffirst tail)) (apply concat tail) tail))
       (-> tail try-generate transform-exprs)))))

;;;

(defn- meta? [expr]
  (instance? clojure.lang.IMeta expr))

(defn-try try-infer
  #(if (or (not (meta? %)) (not *inspector*))
    %
    (do
      (with-meta %
        (assoc ^% :tag (or (*inspector* %) (:tag ^%))))))
  "Error while inferring type")

(defn infer-types [expr]
  (tree-map expr try-infer))

;;;

(defn- tag-assignments [expr]
  (if (not *assignment*)
    expr
    (tree-map expr *assignment*)))

(defn- transform-assignments [expr]
  (let [vars (atom #{})]
    (tree-map expr
      (fn [x]
        (if (not (:assignment ^x))
          x
          (cond
            (sequential? x)
              (do
                (swap! vars #(conj % (last x)))
                x)
            (and (symbol? x) (not (@vars x)))
              (if (:tag ^x)
                (do
                  (swap! vars #(conj % x))
                  (with-meta
                    (list (:tag ^x) (with-meta x (assoc ^x :assignment false)))
                    {:assignment true}))
                (throw (ParseException. (str "Cannot determine type of " x) 0)))
            :else
              x))))))

;;;

(defn parse-lines
  "Maps *parser* over a list of s-expressions, filters out empty lines,
  and optionally adds a terminating character."
  ([exprs] (parse-lines exprs ""))
  ([exprs termination]
     (if (and
          (seq? exprs)
          (= 1 (count exprs))
          (seq? (first exprs)))
      (parse-lines (first exprs) termination)
      (let [exprs             (if (seq? (first exprs)) exprs (list exprs))
            translated-exprs  (map #(*parser* %) exprs)
            filtered-exprs    (filter #(not= (.trim %) "") translated-exprs)
            terminated-exprs  (map #(if (.endsWith % "\n") % (str % termination "\n")) filtered-exprs)]
        (apply str terminated-exprs)))))

(defn-try parse
  #(realize (*parser* %))
  "Error while parsing")

;;;

(defn translate-expr
  "Applies all the multi-fns, in the appropriate order."  
  [expr]
  (-> expr
    transform-exprs
    generate-exprs reverse
    infer-types
    tag-assignments transform-assignments
    parse-lines))

