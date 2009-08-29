;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.translate.core
  (:use [clojure.contrib.pprint])  
  (:import (java.text ParseException))
  (:import (java.io StringWriter))
  (:require [clojure.zip :as zip]))

;;;

(def *generator* nil)
(def *parser* nil)
(def *transformer* nil)

;;;

(defn- realize [s]
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

;;;

(defn- try-transform [expr fun]
  (try
    (realize (fun expr))
    (catch ParseException e
      (throw e))
    (catch Exception e
      (throw (ParseException. (str "\nError while transforming:\n" (str-pprint expr) (.getMessage e)) 0)))))

(defn tree-map [tree fun]
  (if (empty? tree)
    ()
    (loop [z (zip/seq-zip tree)]
      (if (zip/end? z)
        (zip/root z)
        (recur (zip/next (zip/replace z (try-transform (zip/node z) fun))))))))

(defn- transform-exprs [expr]
  (tree-map expr *transformer*))

;;;

(defn try-generate [expr]
  (try
    (realize (*generator* expr))
    (catch Exception e
      (throw (ParseException. (str "\nError while generating:\n" (str-pprint expr) (.getMessage e)) 0)))))

(defn generate-exprs [expr]
  (loop [body (list expr)
         tail (-> expr try-generate transform-exprs)]
    (if (empty? tail)
     body
     (recur
       (concat body (if (seq? (ffirst tail)) (apply concat tail) tail))
       (-> tail try-generate transform-exprs)))))

;;;

(defn parse-lines
  "Maps fun over a list of s-expressions, filters out empty lines,
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

(defn try-parse [expr fun]
  (try
    (realize (fun expr))
    (catch ParseException e
      (throw e))
    (catch Exception e
      (throw (ParseException. (str "\nError while parsing:\n" (str-pprint expr) (.getMessage e)) 0)))))

;;;

(defn translate-expr [expr]
  (-> expr transform-exprs generate-exprs reverse parse-lines))

