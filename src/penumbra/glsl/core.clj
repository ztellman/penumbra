;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.glsl.core
  (:use [penumbra.translate core])
  (:use [clojure.contrib.def :only (defvar- defmacro-)])
  (:use [clojure.contrib.pprint :only (pprint)])
  (:import (java.text ParseException))
  (:require [penumbra.translate.c :as c] ))

;;;

(defn- parse-keyword
  "Turns :model-view-matrix into gl_ModelViewMatrix."
  [k]
  (symbol
    (str
      "gl_"
      (apply str
        (map
          #(str (.. % (substring 0 1) toUpperCase) (. % substring 1 (count %)))
          (seq (.split (name k) "-")))))))

(def type-map
  (apply hash-map
    '(float float, float2 vec2, float3 vec3, float4 vec4
      int int, int2 ivec2, int3 ivec3, int4 ivec4
      color3 float3, color4 float4)))

(def tuple
  (apply hash-map
    '(float 1, vec2 2, vec3 3, vec4 4
      int 1, ivec2 2, ivec3 3, ivec4 4)))
      
;;;

(defmulti transformer
  (fn [_] nil)
  :default :none)

(defmulti generator
  #(if (seq? %) (first %) nil)
  :default :none)

(defmulti parser
  #(if (seq? %) (first %) nil)
  :default :none)

(defmulti inspector
  #(if (seq? %) (first %) nil)
  :default :none)

(defmethod transformer :none [expr]
  (cond
    (contains? type-map expr) (expr type-map)
    :else                     (c/transformer expr)))

(defmethod generator :none [expr]
  (c/generator expr))

(defmethod parser :none [expr]
  (cond
    (keyword? expr) (parse-keyword expr)
    :else           (c/parser expr)))

(defmethod inspector :none [expr]
  ^expr)

;;;

(defn- typeof [expr]
  (cond
    (integer? expr) 'int
    (float? expr)   'float
    :else           (:tag ^expr)))

(defn- def-constant-inspector [[k v]]
  `(defmethod inspector ~k [x#]
     ~v))

(defmacro- def-constant-inspectors [& s]
  (let [pairs (partition 2 s)
        fns (map def-constant-inspector pairs)]
    `(do ~@fns)))

(defn- def-identity-inspector [sym]
  `(defmethod inspector ~sym [x#]
     (let [types# (map typeof (next x#))]
       (if (apply not= types#)
         (throw (ParseException. (str "Mismatched types in " (list* x#) " : inferrered types are " (list* types#)) 0))
         (first types#)))))

(defmacro- def-identity-inspectors [& symbols]
  (let [fns (map def-identity-inspector symbols)]
    `(do ~@fns)))

(defn- def-maximum-inspector [sym]
  `(defmethod inspector ~sym [x#]
     (let [types# (map typeof (next x#))
           maximum# (apply max (map tuple types#))]
       (first (filter (fn [p#] (= maximum# (tuple p#))) types#)))))

(defmacro- def-maximum-inspectors [& symbols]
  (let [fns (map def-maximum-inspector symbols)]
    `(do ~@fns)))
       

;;;

(def-constant-inspectors
  'vec4 'vec4
  'vec3 'vec3
  'vec2 'vec2
  'float 'float
  '.x 'float, '.xy 'float2, '.xyz 'float3, '.xyzw 'float4)

(def-identity-inspectors
  '+ '- 'normalize 'cos 'sin 'max 'min 'floor 'fract 'ceil)

(def-maximum-inspectors
  '* '/)

;;;

(defn translate-declarations [decl]
  (if (empty? decl)
    ""
    (parse-lines (map #(list 'declare %) decl) ";")))

(defn translate-glsl [expr]
  (binding [*transformer* transformer, *generator* generator, *parser* parser, *inspector* inspector, *assignment* c/assignment]
    (translate-expr expr)))

(defn translate-shader
  ([exprs] (translate-shader '() exprs))
  ([decl exprs]
    (binding [*transformer* transformer, *generator* generator, *parser* parser]
      (let [exprs (tree-map exprs #(if (keyword? %) (parse-keyword %) %))]
      (str
        (translate-declarations decl)
        (translate-expr
          (concat
            '(defn void main [])
            (list 'do exprs))))))))