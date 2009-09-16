;   Copyright (c) Zachary Tellman. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns penumbra.glsl.core
  (:use [penumbra.translate core])
  (:use [clojure.contrib.def :only (defvar defvar- defmacro-)])
  (:use [clojure.contrib.pprint :only (pprint)])
  (:import (java.text ParseException))
  (:require [penumbra.translate.c :as c]))

;;;

(defn- parse-keyword
  "Turns :model-view-matrix into gl_ModelViewMatrix."
  [k]
  (str
   "gl_"
   (apply
    str
    (map
     #(str (.. % (substring 0 1) toUpperCase) (. % substring 1 (count %)))
     (seq (.split (name k) "-"))))))

(defvar type-map
  (apply hash-map
    '(float float, float2 vec2, float3 vec3, float4 vec4
      int int, int2 ivec2, int3 ivec3, int4 ivec4
      color float, color2 vec2, color3 vec3, color4 vec4)))

(defvar- tuple
  (apply hash-map
    '(float 1, float2 2, float3 3, float4 4
      int 1, int2 2, int3 3, int4 4
      color 1, color2 2, color3 3, color4 4)))
      
;;;

(defmulti transformer
  #(if (seq? %) (first %) nil)
  :default :none)

(defmulti generator
  #(if (seq? %) (first %) nil)
  :default :none)

(defmulti parser
  #(if (seq? %) (first %) nil)
  :default :none)

(defmulti inspector
  #(cond
     (not (seq? %)) nil
     (c/swizzle? %) :swizzle
     :else (first %))
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

;;;

(defmethod inspector :none [expr]
  (let [transformed-type (type-map (:tag ^expr))]
    (if transformed-type
      transformed-type
      (typeof expr))))

(defmethod inspector :swizzle [expr]
  (let [swizzle (-> expr first name rest)
        tuple   (-> expr first name count dec)
        type    (-> expr second meta :tag)]
    (if (and
          type
          (or (every? (set "rgba") swizzle)
              (every? (set "xyzw") swizzle)
              (every? (set "stqr") swizzle)))
      (let [subtype (-> type name (.substring 0 (-> type name count dec)))]
        (symbol (str subtype (if (= 1 tuple) "" tuple))))
      nil)))

(defn- def-constant-inspector [[k v]]
  `(defmethod inspector ~k [x#]
     ~v))

(defmacro- def-constant-inspectors [& s]
  (let [pairs (partition 2 s)
        fns (map def-constant-inspector pairs)]
    `(do ~@fns)))

(defn- def-identity-inspector [sym]
  `(defmethod inspector ~sym [x#]
    (let [types# (filter identity (map typeof (next x#)))]
       (if (empty? types#)
         nil
         (if (apply not= types#)
          (throw (ParseException. (str "Mismatched types in " (list* x#) " : inferrered types are " (list* types#)) 0))
          (first types#))))))

(defmacro- def-identity-inspectors [& symbols]
  (let [fns (map def-identity-inspector symbols)]
    `(do ~@fns)))

(defn- def-maximum-inspector [sym]
  `(defmethod inspector ~sym [expr#]
     (let [types# (map typeof (next expr#))
           known-types# (filter identity types#)]
       (if (or (empty? known-types#) (not= (count types#) (count known-types#))) 
         nil
         (let [maximum# (apply max (map tuple types#))]
           (first (filter (fn [p#] (= maximum# (tuple p#))) types#)))))))

(defmacro- def-maximum-inspectors [& symbols]
  (let [fns (map def-maximum-inspector symbols)]
    `(do ~@fns)))
       
;;;

(def-constant-inspectors
  'vec4 'float4
  'vec3 'float3
  'vec2 'float2
  'float 'float
  'noise4 'float4
  'noise3 'float3
  'noise2 'float2,
  'noise1 'float
  'dot 'float
  'texture2DRect 'float4
  '< 'bool
  '> 'bool
  '= 'bool
  '<= 'bool
  '>= 'bool)

(def-identity-inspectors
  '+ '- 'normalize 'cos 'sin 'max 'min 'floor 'fract 'ceil 'abs)

(def-maximum-inspectors
  '* '/ 'mix 'pow)

;;;

(defn- def-transform-modifier [sym]
  `(defmethod transformer ~sym [expr#]
     (let [x# (second expr#)]
       (add-meta x# :modifiers (into (:modifiers ^x#) [~sym])))))

(defmacro- def-transform-modifiers [& symbols]
  (let [fns (map def-transform-modifier symbols)]
    `(do ~@fns)))

(def-transform-modifiers 'in, 'out, 'inout, 'uniform, 'attribute, 'varying)

;;;

(defn transform-tags [expr]
  (tree-map
   expr
   #(if (meta? %)
     (add-meta % :tag (or (type-map (:tag ^%)) (:tag ^%)))
     %)))

(defn translate-glsl [expr]
  (binding [*transformer* transformer, *generator* generator, *parser* parser, *inspector* inspector, *tagger* c/tagger]
    (-> expr transform-expr transform-tags parser)))

(defn transform-glsl [expr]
  (binding [*transformer* transformer, *generator* generator, *inspector* inspector, *tagger* c/tagger]
    (-> expr transform-expr)))

(defn translate-shader
  ([expr]
     (translate-glsl expr))
  ([decl exprs]
     (translate-shader
      (list
       (list
        'do
        (map #(list 'declare %) decl))
       (list
        'defn 'void 'main []
        (list 'do exprs))))))