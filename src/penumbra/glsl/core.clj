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

(def type-map
  {:float :float, :float2 :vec2, :float3 :vec3, :float4 :vec4
   :int :int, :int2 :ivec2, :int3 :ivec3, :int4 :ivec4
   :color :float, :color2 :vec2, :color3 :vec3, :color4 :vec4})

(def type-tuple
  {:float 1, :float2 2, :float3 3, :float4 4
   :int 1, :int2 2, :int3 3, :int4 4
   :color 1, :color2 2, :color3 3, :color4 4})
      
;;;

(defmulti transformer
  #(if (seq? %) (id (first %)) nil)
  :default nil)

(defmulti generator
  #(if (seq? %) (id (first %)) nil)
  :default nil)

(defmulti parser
  #(if (seq? %) (id (first %)) nil)
  :default nil)

(defmulti inspector
  #(cond
    (keyword? %) %
    (not (seq? %)) nil
    (c/swizzle? %) :swizzle
    :else (id (first %)))
  :default nil)

(defmethod transformer nil [x]
  (c/transformer x))

(defmethod generator nil [x]
  (c/generator x))

(defmethod parser nil [x]
  (cond
    (keyword? x) (parse-keyword x)
    :else           (c/parser x)))

;;;

(defmethod inspector nil [x]
  (when-not (keyword? x)
    (let [transformed-type (type-map (:tag (meta x)))]
      (if transformed-type
        transformed-type
        (typeof x)))))

(defmethod inspector :swizzle [x]
  (let [swizzle (-> x first name rest)
        tuple   (-> x first name count dec)
        type    (-> x second typeof)]
    (if (and
          type
          (or (every? (set "rgba") swizzle)
              (every? (set "xyzw") swizzle)
              (every? (set "stqr") swizzle)))
      (let [subtype (->> type name (re-find #"[a-z]*"))]
        (keyword (str subtype (if (= 1 tuple) "" tuple))))
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
  `(defmethod inspector ~sym [x#]
     (let [types# (map typeof (next x#))
           known-types# (filter identity types#)]
       (if (or (empty? known-types#) (not= (count types#) (count known-types#))) 
         nil
         (let [maximum# (apply max (map type-tuple known-types#))]
           (first (filter (fn [p#] (= maximum# (type-tuple p#))) known-types#)))))))

(defmacro- def-maximum-inspectors [& symbols]
  (let [fns (map def-maximum-inspector symbols)]
    `(do ~@fns)))
       
;;;

(def-constant-inspectors
  'float4 :float4
  'float3 :float3
  'float2 :float2
  'float :float
  'color4 :color4
  'color3 :color3
  'color2 :color2
  'color :color
  'noise4 :float4
  'noise3 :float3
  'noise2 :float2
  'noise1 :float
  'int :int
  'int2 :int2
  'int3 :int3
  'int4 :int4
  'dot :float
  'length :float
  'texture1DRect :float4
  'texture1D :float4
  'texture2DRect :float4
  'texture2D :float4
  'texture3DRect :float4
  'texture3D :float4
  'lighting :float4
  '< :bool
  '> :bool
  '= :bool
  '<= :bool
  '>= :bool
  :coord :float2
  :dim :float2
  :index :float)

(def-identity-inspectors
  '+ '- 'normalize 'cos 'sin 'max 'min 'floor 'fract 'ceil 'abs 'log 'log2 'sqrt 'mod)

(def-maximum-inspectors
  '* 'div 'mix 'pow)

(defmethod inspector 'if [x]
  (if (= 3 (count x))
    (typeof (nth x 2))
    (or (typeof (nth x 2)) (typeof (nth x 3)))))

;;;

(defn- def-transform-modifier [sym]
  `(defmethod transformer ~sym [x#]
     (if (= 2 (count x#))
       (let [x# (second x#)]
         (add-meta x# :modifiers (into (:modifiers (meta x#)) [~sym])))
       (let [type# (second x#)
             x# (nth x# 2)]
         (add-meta x# :modifiers (into (:modifiers (meta x#)) [~sym]) :tag (keyword type#))))))

(defmacro- def-transform-modifiers [& symbols]
  (let [fns (map def-transform-modifier symbols)]
    `(do ~@fns)))

(def-transform-modifiers 'in 'out 'inout 'uniform 'attribute 'varying)

(defmethod transformer 'nth [x]
  (concat
   (take (-> x count dec) x)
   (if (or (-> x last number?) (-> x last typeof (= :int)))
     (list (int (last x)))
     (list (list 'int (last x))))))

(defmethod transformer 'lighting [x]
  (list* 'lighting (list 'int (second x)) (nnext x)))

;;;

(defn transform-numbers [x]
  (let [suffix {1 "", 2 "2", 3 "3", 4 "4"}]
    (->> x
         (tree-map #(when (number? %) (float %)))
         (tree-map
          #(when (and (vector? %) (< 0 (count %)) (every? number? %))
             (list* (symbol (str "float" (suffix (count %)))) %))))))

;;;

(defn transform-constructors [x]
  (tree-map
   #(if (and (symbol? %) (-> % keyword type-map))
      (-> % keyword type-map name symbol))
   x))

(defn- transform-tags [x]
  (tree-map
   #(if-let [type (:tag (meta %))]
      (add-meta % :tag (or (type-map type) type))
      %)
   x))

(defn transform-glsl [x]
  (binding [*preprocessor* transform-numbers
            *transformer* transformer
            *generator* generator
            *inspector* inspector
            *tagger* c/tagger]
    (->> x transform-expr transform-tags transform-constructors)))

(defn translate-glsl [x]
  (try-translate
   (binding [*preprocessor* transform-numbers
             *transformer* transformer
             *generator* generator
             *parser* parser
             *inspector* inspector
             *tagger* c/tagger]
     (->> x transform-glsl parser))))

(defn translate-shader
  ([x]
     (translate-glsl x))
  ([decl x]
     (translate-shader
      (list
       (list
        'do
        (map #(list 'declare %) decl))
       (list
        'defn 'void 'main []
        (list 'do x))))))
